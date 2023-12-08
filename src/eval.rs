use std::{
    any::Any,
    collections::HashMap,
    error::Error,
    fmt::Display,
    ops::{Index, IndexMut},
    time::UNIX_EPOCH,
};

use crate::{
    ast::Operator,
    compile::{Chunk, ChunkIndex, Compiler, Const, Instr, RegIndex, Symbol},
    gc::{Addr, Allocator},
};

// both underlying (Rust) type and lifetime are dynamical
pub type Dyn = Box<dyn Any + Send + Sync>;

pub type Record = HashMap<Symbol, Value>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Invalid,
    // universal representation
    Dyn(Addr),
    // extension to support representing closures with records
    ChunkIndex(usize),
    // extension for common efficiency
    Unit,
    I32(i32),
    U64(u64),
    F64(f64),
}

impl Value {
    fn type_name(&self) -> String {
        format!("{self:?}")
    }
}

#[derive(Debug)]
pub struct Evaluator {
    chunks: Vec<Chunk>,
    allocator: Allocator,

    chunk_symbol: Symbol,
    intrinsics: HashMap<ChunkIndex, Intrinsic>,

    registers: Vec<Value>,
    frames: Vec<Frame>,
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
    ApplyTypeError(String),
    Operator2TypeError(String, String),
    Operator1TypeError(String),
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
        let chunk_symbol = compiler.intern("%chunk".into());
        let mut lang_chunks = HashMap::<_, Intrinsic>::new();
        lang_chunks.insert(compiler.lang_indexes["print"], Self::intrinsic_print);
        lang_chunks.insert(compiler.lang_indexes["clock"], Self::intrinsic_clock);
        lang_chunks.insert(compiler.lang_indexes["repr"], Self::intrinsic_repr);
        Self {
            chunks: compiler.chunks,
            allocator,
            chunk_symbol,
            intrinsics: lang_chunks,
            registers: Default::default(),
            frames: Default::default(),
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
                    record.insert(self.chunk_symbol, Value::ChunkIndex(*chunk_index));
                    r[i] = Value::Dyn(self.allocator.alloc(record))
                }
                Instr::LoadField(i, j, symbol) => {
                    let Value::Dyn(addr) = &r[j] else {
                        err(EvalErrorKind::RecordTypeError(r[j].type_name()))?
                    };
                    let Some(record) = addr.as_ref().downcast_ref::<Record>() else {
                        err(EvalErrorKind::RecordTypeError(r[j].type_name()))?
                    };
                    r[i] = record[symbol].clone()
                }
                Instr::StoreField(i, symbol, j) => {
                    let value = r[j].clone();
                    let Value::Dyn(addr) = &mut r[i] else {
                        err(EvalErrorKind::RecordTypeError(r[i].type_name()))?
                    };
                    let Some(record) = addr.as_mut().downcast_mut::<Record>() else {
                        err(EvalErrorKind::RecordTypeError(r[i].type_name()))?
                    };
                    if record.contains_key(&self.chunk_symbol) {
                        err(EvalErrorKind::RecordTypeError(r[i].type_name()))?
                    }
                    record.insert(*symbol, value);
                }
                Instr::Move(i, j) => r[j] = r[i].clone(),
                Instr::Operator(i, op, xs) => {
                    r[i] = if xs.len() == 2 {
                        match (op, &r[&xs[0]], &r[&xs[1]]) {
                            (Operator::Add, Value::I32(a), Value::I32(b)) => Value::I32(*a + *b),
                            (Operator::Sub, Value::I32(a), Value::I32(b)) => Value::I32(*a - *b),
                            (Operator::Mul, Value::I32(a), Value::I32(b)) => Value::I32(*a * *b),
                            (Operator::Div, Value::I32(a), Value::I32(b)) => Value::I32(*a / *b),
                            (Operator::Rem, Value::I32(a), Value::I32(b)) => Value::I32(*a % *b),
                            (Operator::Add, Value::Dyn(a), Value::Dyn(b)) => {
                                if let (Some(a), Some(b)) = (
                                    a.as_ref().downcast_ref::<String>(),
                                    b.as_ref().downcast_ref::<String>(),
                                ) {
                                    Value::Dyn(self.allocator.alloc([&**a, &**b].concat()))
                                } else {
                                    err(EvalErrorKind::Operator2TypeError(
                                        r[&xs[0]].type_name(),
                                        r[&xs[1]].type_name(),
                                    ))?
                                }
                            }
                            _ => err(EvalErrorKind::Operator2TypeError(
                                r[&xs[0]].type_name(),
                                r[&xs[1]].type_name(),
                            ))?,
                        }
                    } else if xs.len() == 1 {
                        match (op, &r[&xs[0]]) {
                            (Operator::Neg, Value::I32(a)) => Value::I32(-*a),
                            _ => err(EvalErrorKind::Operator1TypeError(r[&xs[0]].type_name()))?,
                        }
                    } else {
                        unreachable!()
                    }
                }
                Instr::Apply(i, arity) => {
                    let Value::Dyn(addr) = r[i] else {
                        err(EvalErrorKind::ApplyTypeError(r[i].type_name()))?
                    };
                    let Some(record) = addr.as_ref().downcast_ref::<Record>() else {
                        err(EvalErrorKind::ApplyTypeError(r[i].type_name()))?
                    };
                    let Some(Value::ChunkIndex(chunk_index)) = record.get(&self.chunk_symbol)
                    else {
                        err(EvalErrorKind::ApplyTypeError(r[i].type_name()))?
                    };
                    if *arity != self.chunks[*chunk_index].arity {
                        err(EvalErrorKind::ArityError(
                            *arity,
                            self.chunks[*chunk_index].arity,
                        ))?
                    }
                    if let Some(intrinsic) = self.intrinsics.get(chunk_index) {
                        intrinsic(self)?
                    } else {
                        let frame = Frame {
                            chunk_index: *chunk_index,
                            base_pointer: *i as _,
                            instr_pointer: 0,
                        };
                        self.frames.push(frame)
                    }
                }
                Instr::MatchField(i, symbol, instr) => {
                    let Value::Dyn(addr) = r[i] else {
                        err(EvalErrorKind::RecordTypeError(r[i].type_name()))?
                    };
                    let Some(record) = addr.as_ref().downcast_ref::<Record>() else {
                        err(EvalErrorKind::RecordTypeError(r[i].type_name()))?
                    };
                    if record.contains_key(&self.chunk_symbol) {
                        err(EvalErrorKind::RecordTypeError(r[i].type_name()))?
                    }
                    if !record.contains_key(symbol) {
                        frame.instr_pointer = *instr
                    }
                }
                Instr::Jump(instr) => frame.instr_pointer = *instr,
            }
        }
        Ok(())
    }

    fn intrinsic_print(&mut self) -> anyhow::Result<()> {
        let mut r = I(
            &mut self.registers,
            self.frames.last().unwrap().base_pointer,
        );
        let Value::Dyn(addr) = r[0] else {
            Err(EvalErrorKind::TypeError(r[0].type_name(), "String".into()))?
        };
        let Some(s) = addr.as_ref().downcast_ref::<String>() else {
            Err(EvalErrorKind::TypeError(r[0].type_name(), "String".into()))?
        };
        println!("{s}");
        r[0] = Value::Unit;
        Ok(())
    }

    fn intrinsic_clock(&mut self) -> anyhow::Result<()> {
        let mut r = I(
            &mut self.registers,
            self.frames.last().unwrap().base_pointer,
        );
        r[0] = Value::F64(UNIX_EPOCH.elapsed().unwrap().as_secs_f64());
        Ok(())
    }

    fn intrinsic_repr(&mut self) -> anyhow::Result<()> {
        let mut r = I(
            &mut self.registers,
            self.frames.last().unwrap().base_pointer,
        );
        let repr = format!("{:?}", r[0]);
        let addr = self.allocator.alloc(repr);
        r[0] = Value::Dyn(addr);
        Ok(())
    }
}
