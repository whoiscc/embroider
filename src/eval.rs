use std::{
    collections::HashMap,
    error::Error,
    fmt::Display,
    ops::{Index, IndexMut},
    sync::Arc,
};

use crate::{
    ast::Operator,
    compile::{Chunk, ChunkIndex, Compiler, Const, Instr, RegIndex, Symbol},
    gc::Allocator,
    sched::{Control, Worker},
    value::{self, Record},
    Value,
};

#[derive(Debug)]
pub struct Evaluator {
    consts: Arc<EvaluatorConsts>,
    pub allocator: Allocator,
    pub registers: Vec<Value>,
    pub intrinsic_base_pointer: usize,
    frames: Vec<Frame>,
}

#[derive(Debug)]
pub struct EvaluatorConsts {
    chunks: Vec<Chunk>,
    lang_indexes: HashMap<String, ChunkIndex>,
    symbols: Vec<String>,
    intrinsics: HashMap<ChunkIndex, Intrinsic>,
    symbol_chunk: Symbol,
    symbol_true: Symbol,
    symbol_false: Symbol,
}

type Intrinsic = fn(&mut Evaluator) -> Result<(), EvalErrorKind>;

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
    RecordKeyError(String),
    Operator2TypeError(Operator, String, String),
    Operator1TypeError(Operator, String),
    ArityError(usize, usize),
    SpawnIntrinsicError(ChunkIndex),
    Panic(String),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Error for EvalError {}

impl Evaluator {
    pub fn new(consts: Arc<EvaluatorConsts>, allocator: Allocator) -> Self {
        assert!(
            consts.lang_indexes.is_empty(),
            "language items not linked: {:?}",
            consts.lang_indexes
        );
        Self {
            consts,
            allocator,
            registers: Default::default(),
            frames: Default::default(),
            intrinsic_base_pointer: 0,
        }
    }
}

impl EvaluatorConsts {
    pub fn new(mut compiler: Compiler) -> Self {
        let mut this = Self {
            symbol_chunk: compiler.intern("%chunk".into()),
            symbol_true: compiler.intern("True".into()),
            symbol_false: compiler.intern("False".into()),
            chunks: compiler.chunks,
            symbols: compiler.symbols,
            lang_indexes: compiler.lang_indexes,
            intrinsics: Default::default(),
        };
        this.link("print", Evaluator::intrinsic_print);
        this.link("repr", Evaluator::intrinsic_repr);
        this.link("panic", Evaluator::intrinsic_panic);
        value::link(&mut this);
        this
    }

    pub fn link(&mut self, name: &str, intrinsic: Intrinsic) -> bool {
        if let Some(index) = self.lang_indexes.remove(name) {
            self.intrinsics.insert(index, intrinsic);
            true
        } else {
            false
        }
    }
}

pub struct I<'a>(pub &'a mut Vec<Value>, pub usize);

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
    pub fn push_entry_frame(&mut self, chunk_index: usize) {
        assert!(self.frames.is_empty());
        self.frames.push(Frame {
            chunk_index,
            base_pointer: 0,
            instr_pointer: 0,
        })
    }

    pub fn push_entry_value(&mut self, value: Value) {
        assert!(self.registers.is_empty());
        self.registers.push(value)
    }

    pub fn eval(&mut self, worker: &mut Worker) -> anyhow::Result<()> {
        while let Some(frame) = self.frames.last_mut() {
            let chunk = &self.consts.chunks[frame.chunk_index];
            let Some(instr) = chunk.instrs.get(frame.instr_pointer) else {
                assert_eq!(frame.instr_pointer, chunk.instrs.len());
                self.registers.truncate(frame.base_pointer + 1); // for returned value
                self.frames.pop().unwrap();
                continue;
            };

            let err = {
                let chunk_index = frame.chunk_index;
                let instr_pointer = frame.instr_pointer;
                move |kind| EvalError(kind, chunk_index, instr_pointer)
            };
            frame.instr_pointer += 1;
            let mut r = I(&mut self.registers, frame.base_pointer);
            // println!("{instr:?}");
            match instr {
                Instr::LoadUnit(i) => r[i] = Value::Unit,
                Instr::LoadConst(i, const_index) => {
                    r[i] = match chunk.consts[*const_index].clone() {
                        Const::I32(integer) => Value::I32(integer),
                        Const::String(string) => {
                            Value::Dyn(self.allocator.alloc(value::String(string)))
                        }
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
                    record.insert(self.consts.symbol_chunk, Value::ChunkIndex(*chunk_index));
                    r[i] = Value::Dyn(self.allocator.alloc(record))
                }
                Instr::LoadField(i, j, symbol) => {
                    let rj = r[j].downcast_ref::<Record>().map_err(err)?;
                    if let Some(value) = rj.get(symbol) {
                        r[i] = value.clone()
                    } else {
                        Err(err(EvalErrorKind::RecordKeyError(
                            self.consts.symbols[*symbol].clone(),
                        )))?
                    }
                }
                Instr::StoreField(i, symbol, j) => {
                    let value = r[j].clone();
                    let ri = r[i].downcast_mut::<Record>().map_err(err)?;
                    let evicted = ri.insert(*symbol, value);
                    if evicted.is_none() && !ri.contains_key(&self.consts.symbol_chunk) {
                        Err(err(EvalErrorKind::RecordKeyError(
                            self.consts.symbols[*symbol].clone(),
                        )))?
                    }
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

                            (Operator::Add, Value::U64(a), Value::U64(b)) => Value::U64(*a + *b),
                            (Operator::Sub, Value::U64(a), Value::U64(b)) => Value::U64(*a - *b),
                            (Operator::Mul, Value::U64(a), Value::U64(b)) => Value::U64(*a * *b),
                            (Operator::Div, Value::U64(a), Value::U64(b)) => Value::U64(*a / *b),
                            (Operator::Rem, Value::U64(a), Value::U64(b)) => Value::U64(*a % *b),
                            (Operator::Eq, Value::U64(a), Value::U64(b)) => Value::Bool(*a == *b),
                            (Operator::Ne, Value::U64(a), Value::U64(b)) => Value::Bool(*a != *b),
                            (Operator::Lt, Value::U64(a), Value::U64(b)) => Value::Bool(*a < *b),
                            (Operator::Gt, Value::U64(a), Value::U64(b)) => Value::Bool(*a > *b),
                            (Operator::Le, Value::U64(a), Value::U64(b)) => Value::Bool(*a <= *b),
                            (Operator::Ge, Value::U64(a), Value::U64(b)) => Value::Bool(*a >= *b),
                            (Operator::Lsh, Value::U64(a), Value::U64(b)) => Value::U64(*a << *b),
                            (Operator::Rsh, Value::U64(a), Value::U64(b)) => Value::U64(*a >> *b),
                            (Operator::Xor, Value::U64(a), Value::U64(b)) => Value::U64(*a ^ *b),
                            (Operator::Band, Value::U64(a), Value::U64(b)) => Value::U64(*a & *b),
                            (Operator::Bor, Value::U64(a), Value::U64(b)) => Value::U64(*a | *b),

                            (Operator::Add, a, b) => {
                                if let (Ok(a), Ok(b)) = (
                                    a.downcast_ref::<value::String>(),
                                    b.downcast_ref::<value::String>(),
                                ) {
                                    Value::Dyn(
                                        self.allocator
                                            .alloc(value::String(String::from(a.clone()) + b)),
                                    )
                                } else {
                                    Err(err(EvalErrorKind::Operator2TypeError(
                                        *op,
                                        r[&xs[0]].type_name().into(),
                                        r[&xs[1]].type_name().into(),
                                    )))?
                                }
                            }
                            _ => Err(err(EvalErrorKind::Operator2TypeError(
                                *op,
                                r[&xs[0]].type_name().into(),
                                r[&xs[1]].type_name().into(),
                            )))?,
                        }
                    } else if xs.len() == 1 {
                        match (op, &r[&xs[0]]) {
                            (Operator::Neg, Value::I32(a)) => Value::I32(-*a),
                            _ => Err(err(EvalErrorKind::Operator1TypeError(
                                *op,
                                r[&xs[0]].type_name().into(),
                            )))?,
                        }
                    } else {
                        unreachable!()
                    }
                }
                Instr::Apply(i, arity) => {
                    let ri = r[i].downcast_ref::<Record>().map_err(err)?;
                    let Some(Value::ChunkIndex(chunk_index)) = ri.get(&self.consts.symbol_chunk)
                    else {
                        Err(err(EvalErrorKind::TypeError(
                            "Record".into(),
                            "(Abstraction)".into(),
                        )))?
                    };
                    if *arity != self.consts.chunks[*chunk_index].arity {
                        Err(err(EvalErrorKind::ArityError(
                            *arity,
                            self.consts.chunks[*chunk_index].arity,
                        )))?
                    }
                    if let Some(intrinsic) = self.consts.intrinsics.get(chunk_index) {
                        self.intrinsic_base_pointer = frame.base_pointer + *i as usize;
                        intrinsic(self).map_err(err)?
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
                            *symbol == self.consts.symbol_true
                        } else {
                            *symbol == self.consts.symbol_false
                        }
                    } else {
                        let ri = r[i].downcast_ref::<Record>().map_err(err)?;
                        if ri.contains_key(&self.consts.symbol_chunk) {
                            Err(err(EvalErrorKind::TypeError(
                                "(Abstraction)".into(),
                                "Record".into(),
                            )))?
                        }
                        ri.contains_key(symbol)
                    };
                    if !matched {
                        frame.instr_pointer = *instr
                    }
                }
                Instr::Jump(instr) => frame.instr_pointer = *instr,
                Instr::Spawn(i) => {
                    let ri = r[i].downcast_ref::<Record>().map_err(err)?;
                    let Some(Value::ChunkIndex(chunk_index)) = ri.get(&self.consts.symbol_chunk)
                    else {
                        Err(err(EvalErrorKind::TypeError(
                            "Record".into(),
                            "(Abstraction)".into(),
                        )))?
                    };
                    if self.consts.chunks[*chunk_index].arity != 0 {
                        Err(err(EvalErrorKind::ArityError(
                            self.consts.chunks[*chunk_index].arity,
                            0,
                        )))?
                    }
                    if self.consts.intrinsics.contains_key(chunk_index) {
                        Err(err(EvalErrorKind::SpawnIntrinsicError(*chunk_index)))?
                    }
                    worker.spawn(*chunk_index, r[i].clone())?
                }
                Instr::LoadControl(i) => {
                    r[i] = Value::Dyn(self.allocator.alloc(worker.new_control()))
                }
                Instr::Suspend(i) => {
                    r[i].downcast_ref::<Control>().map_err(err)?; // just check
                    worker.suspend(r[i].clone());
                    break;
                }
                Instr::Resume(i) => {
                    let ri = r[i].downcast_ref::<Control>().map_err(err)?;
                    ri.resume()?
                }
            }
            // println!("{:?}", self.registers);
        }
        Ok(())
    }

    fn intrinsic_print(&mut self) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut self.registers, self.intrinsic_base_pointer);
        let r1 = r[1].downcast_ref::<value::String>()?;
        println!("{}", &**r1);
        r[0] = Value::Unit;
        Ok(())
    }

    fn intrinsic_repr(&mut self) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut self.registers, self.intrinsic_base_pointer);
        let repr = format!("{:?}", r[1]);
        r[0] = Value::Dyn(self.allocator.alloc(value::String(repr)));
        // println!("{:?}", r[0]);
        // println!("{}", r[0].type_name());
        Ok(())
    }

    fn intrinsic_panic(&mut self) -> Result<(), EvalErrorKind> {
        let r = I(&mut self.registers, self.intrinsic_base_pointer);
        let r1 = r[1].downcast_ref::<value::String>()?;
        Err(EvalErrorKind::Panic(String::from(r1.clone())))
    }
}
