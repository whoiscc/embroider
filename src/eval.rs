use std::{
    collections::HashMap,
    error::Error,
    fmt::Display,
    ops::{Index, IndexMut},
    time::UNIX_EPOCH,
};

use crate::{
    ast::Operator,
    compile::{Chunk, ChunkIndex, Compiler, Const, Instr, RegIndex, Symbol},
    gc::Allocator,
    value::Record,
    Value,
};

#[derive(Debug)]
pub struct Evaluator {
    chunks: Vec<Chunk>,
    symbols: Vec<String>,
    allocator: Allocator,

    intrinsics: HashMap<ChunkIndex, Intrinsic>,
    symbol_chunk: Symbol,
    symbol_true: Symbol,
    symbol_false: Symbol,

    registers: Vec<Value>,
    frames: Vec<Frame>,
    intrinsic_base_pointer: usize,
}

type Intrinsic = fn(&mut Evaluator) -> anyhow::Result<()>;

#[derive(Debug)]
struct Frame {
    chunk_index: usize,
    base_pointer: usize,
    instr_pointer: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalError(pub EvalErrorKind, pub ChunkIndex, pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvalErrorKind {
    TypeError(String, String),
    RecordTypeError(String),
    RecordKeyError(String),
    ApplyTypeError(String),
    Operator2TypeError(Operator, String, String),
    Operator1TypeError(Operator, String),
    ArityError(usize, usize),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Error for EvalError {}

impl Display for EvalErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Error for EvalErrorKind {}

impl Evaluator {
    pub fn new(mut compiler: Compiler, allocator: Allocator) -> Self {
        let mut intrinsics = HashMap::<_, Intrinsic>::new();
        intrinsics.insert(compiler.lang_indexes["print"], Self::intrinsic_print);
        intrinsics.insert(compiler.lang_indexes["clock"], Self::intrinsic_clock);
        intrinsics.insert(compiler.lang_indexes["repr"], Self::intrinsic_repr);
        Self {
            allocator,
            symbol_chunk: compiler.intern("%chunk".into()),
            symbol_true: compiler.intern("True".into()),
            symbol_false: compiler.intern("False".into()),
            chunks: compiler.chunks,
            symbols: compiler.symbols,
            intrinsics,
            registers: Default::default(),
            frames: Default::default(),
            intrinsic_base_pointer: 0,
        }
    }
}

struct I<'a>(&'a mut Vec<Value>, usize);

impl Index<RegIndex> for I<'_> {
    type Output = Value;
    fn index(&self, index: RegIndex) -> &Self::Output {
        &self.0[self.1 + index as usize]
    }
}

impl IndexMut<RegIndex> for I<'_> {
    fn index_mut(&mut self, index: RegIndex) -> &mut Self::Output {
        let offset = self.1 + index as usize;
        if self.0.len() <= offset {
            self.0.resize(offset + 1, Value::Invalid)
        }
        &mut self.0[offset]
    }
}

impl Index<&RegIndex> for I<'_> {
    type Output = <Self as Index<RegIndex>>::Output;
    fn index(&self, index: &RegIndex) -> &Self::Output {
        self.index(*index)
    }
}

impl IndexMut<&RegIndex> for I<'_> {
    fn index_mut(&mut self, index: &RegIndex) -> &mut Self::Output {
        self.index_mut(*index)
    }
}

impl Evaluator {
    pub fn eval_chunk(&mut self, chunk_index: usize) -> anyhow::Result<()> {
        let frame = Frame {
            chunk_index,
            base_pointer: 0,
            instr_pointer: 0,
        };
        self.frames.push(frame);
        while let Some(frame) = self.frames.last_mut() {
            let chunk = &self.chunks[frame.chunk_index];
            let Some(instr) = chunk.instrs.get(frame.instr_pointer) else {
                assert_eq!(frame.instr_pointer, chunk.instrs.len());
                self.frames.pop().unwrap();
                continue;
            };
            frame.instr_pointer += 1;

            let mut r = I(&mut self.registers, frame.base_pointer);
            let err = |kind| Err(EvalError(kind, frame.chunk_index, frame.instr_pointer - 1));
            // println!("{instr:?}");
            match instr {
                Instr::LoadUnit(i) => r[i] = Value::Unit,
                Instr::LoadConst(i, const_index) => {
                    r[i] = match chunk.consts[*const_index].clone() {
                        Const::I32(integer) => Value::I32(integer),
                        Const::String(string) => Value::Dyn(self.allocator.alloc(string)),
                    }
                }
                Instr::LoadRecord(i, rows) => {
                    let record = rows
                        .iter()
                        .map(|(symbol, i)| (*symbol, r[i].clone()))
                        .collect::<Record>();
                    r[i] = Value::Dyn(self.allocator.alloc(record))
                }
                Instr::LoadChunk(i, chunk_index) => {
                    let mut record = HashMap::new();
                    record.insert(self.symbol_chunk, Value::ChunkIndex(*chunk_index));
                    r[i] = Value::Dyn(self.allocator.alloc(record))
                }
                Instr::LoadField(i, j, symbol) => {
                    let Some(record) = r[j].downcast_ref::<Record>() else {
                        err(EvalErrorKind::RecordTypeError(r[j].type_name().into()))?
                    };
                    if let Some(value) = record.get(symbol) {
                        r[i] = value.clone()
                    } else {
                        err(EvalErrorKind::RecordKeyError(self.symbols[*symbol].clone()))?
                    }
                }
                Instr::StoreField(i, symbol, j) => {
                    let value = r[j].clone();
                    let Some(record) = r[i].downcast_mut::<Record>() else {
                        err(EvalErrorKind::RecordTypeError(r[i].type_name().into()))?
                    };
                    record.insert(*symbol, value);
                }
                Instr::Move(i, j) => r[i] = r[j].clone(),
                Instr::Operator(i, op, xs) => {
                    r[i] = if xs.len() == 2 {
                        match (op, &r[&xs[0]], &r[&xs[1]]) {
                            (Operator::Add, Value::I32(a), Value::I32(b)) => Value::I32(*a + *b),
                            (Operator::Sub, Value::I32(a), Value::I32(b)) => Value::I32(*a - *b),
                            (Operator::Mul, Value::I32(a), Value::I32(b)) => Value::I32(*a * *b),
                            (Operator::Div, Value::I32(a), Value::I32(b)) => Value::I32(*a / *b),
                            (Operator::Rem, Value::I32(a), Value::I32(b)) => Value::I32(*a % *b),
                            (Operator::Eq, Value::I32(a), Value::I32(b)) => Value::Bool(*a == *b),
                            (Operator::Ne, Value::I32(a), Value::I32(b)) => Value::Bool(*a != *b),
                            (Operator::Lt, Value::I32(a), Value::I32(b)) => Value::Bool(*a < *b),
                            (Operator::Gt, Value::I32(a), Value::I32(b)) => Value::Bool(*a > *b),
                            (Operator::Le, Value::I32(a), Value::I32(b)) => Value::Bool(*a <= *b),
                            (Operator::Ge, Value::I32(a), Value::I32(b)) => Value::Bool(*a >= *b),
                            (Operator::Add, Value::F64(a), Value::F64(b)) => Value::F64(*a + *b),
                            (Operator::Sub, Value::F64(a), Value::F64(b)) => Value::F64(*a - *b),
                            (Operator::Mul, Value::F64(a), Value::F64(b)) => Value::F64(*a * *b),
                            (Operator::Div, Value::F64(a), Value::F64(b)) => Value::F64(*a / *b),
                            (Operator::Rem, Value::F64(a), Value::F64(b)) => Value::F64(*a % *b),
                            (Operator::Add, a, b) => {
                                if let (Some(a), Some(b)) =
                                    (a.downcast_ref::<String>(), b.downcast_ref::<String>())
                                {
                                    Value::Dyn(self.allocator.alloc([&**a, &**b].concat()))
                                } else {
                                    err(EvalErrorKind::Operator2TypeError(
                                        *op,
                                        r[&xs[0]].type_name().into(),
                                        r[&xs[1]].type_name().into(),
                                    ))?
                                }
                            }
                            _ => err(EvalErrorKind::Operator2TypeError(
                                *op,
                                r[&xs[0]].type_name().into(),
                                r[&xs[1]].type_name().into(),
                            ))?,
                        }
                    } else if xs.len() == 1 {
                        match (op, &r[&xs[0]]) {
                            (Operator::Neg, Value::I32(a)) => Value::I32(-*a),
                            _ => err(EvalErrorKind::Operator1TypeError(
                                *op,
                                r[&xs[0]].type_name().into(),
                            ))?,
                        }
                    } else {
                        unreachable!()
                    }
                }
                Instr::Apply(i, arity) => {
                    let Some(record) = r[i].downcast_ref::<Record>() else {
                        err(EvalErrorKind::ApplyTypeError(r[i].type_name().into()))?
                    };
                    let Some(Value::ChunkIndex(chunk_index)) = record.get(&self.symbol_chunk)
                    else {
                        err(EvalErrorKind::ApplyTypeError(r[i].type_name().into()))?
                    };
                    if *arity != self.chunks[*chunk_index].arity {
                        err(EvalErrorKind::ArityError(
                            *arity,
                            self.chunks[*chunk_index].arity,
                        ))?
                    }
                    if let Some(intrinsic) = self.intrinsics.get(chunk_index) {
                        self.intrinsic_base_pointer = frame.base_pointer + *i as usize;
                        intrinsic(self)?
                    } else {
                        let frame = Frame {
                            chunk_index: *chunk_index,
                            base_pointer: frame.base_pointer + *i as usize,
                            instr_pointer: 0,
                        };
                        // println!("{frame:?}");
                        self.frames.push(frame)
                    }
                }
                Instr::MatchField(i, symbol, instr) => {
                    let matched = if let Value::Bool(b) = r[i] {
                        if b {
                            *symbol == self.symbol_true
                        } else {
                            *symbol == self.symbol_false
                        }
                    } else {
                        let Some(record) = r[i].downcast_ref::<Record>() else {
                            err(EvalErrorKind::RecordTypeError(r[i].type_name().into()))?
                        };
                        if record.contains_key(&self.symbol_chunk) {
                            err(EvalErrorKind::RecordTypeError(r[i].type_name().into()))?
                        }
                        record.contains_key(symbol)
                    };
                    if !matched {
                        frame.instr_pointer = *instr
                    }
                }
                Instr::Jump(instr) => frame.instr_pointer = *instr,
            }
            // println!("{:?}", self.registers);
        }
        Ok(())
    }

    fn intrinsic_print(&mut self) -> anyhow::Result<()> {
        let mut r = I(&mut self.registers, self.intrinsic_base_pointer);
        let Some(s) = r[1].downcast_ref::<String>() else {
            Err(EvalErrorKind::TypeError(
                r[1].type_name().into(),
                "String".into(),
            ))?
        };
        println!("{s}");
        r[0] = Value::Unit;
        Ok(())
    }

    fn intrinsic_clock(&mut self) -> anyhow::Result<()> {
        let mut r = I(&mut self.registers, self.intrinsic_base_pointer);
        r[0] = Value::F64(UNIX_EPOCH.elapsed().unwrap().as_secs_f64());
        Ok(())
    }

    fn intrinsic_repr(&mut self) -> anyhow::Result<()> {
        let mut r = I(&mut self.registers, self.intrinsic_base_pointer);
        let repr = format!("{:?}", r[1]);
        r[0] = Value::Dyn(self.allocator.alloc(repr));
        // println!("{:?}", r[0]);
        // println!("{}", r[0].type_name());
        Ok(())
    }
}
