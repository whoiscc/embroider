use std::{
    any::Any,
    collections::HashMap,
    ops::{Index, IndexMut},
};

use crate::{
    ast::Operator,
    compile::{Chunk, Compiler, Const, Instr, RegIndex, Symbol},
    gc::{Addr, Allocator},
};

// both underlying (Rust) type and lifetime are dynamical
pub type Dyn = Box<dyn Any + Send + Sync>;

pub type Record = HashMap<Symbol, Value>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Value {
    #[default]
    Invalid,
    // universal representation
    Dyn(Addr),
    // extension to support representing closures with records
    ChunkIndex(usize),
    // extension for common efficiency
    Unit,
    I32(i32),
    U64(u64),
}

#[derive(Debug)]
pub struct Evaluator {
    chunks: Vec<Chunk>,
    allocator: Allocator,

    chunk_symbol: Symbol,

    registers: Vec<Value>,
    frames: Vec<Frame>,
}

#[derive(Debug)]
struct Frame {
    chunk_index: usize,
    base_pointer: usize,
    instr_pointer: usize,
}

impl Evaluator {
    pub fn new(mut compiler: Compiler, allocator: Allocator) -> Self {
        let chunk_symbol = compiler.intern("%chunk".into());
        Self {
            chunks: compiler.chunks,
            allocator,
            chunk_symbol,
            registers: Default::default(),
            frames: Default::default(),
        }
    }

    pub fn eval_chunk(&mut self, chunk_index: usize) {
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
            struct I<'a>(&'a mut Vec<Value>, usize);
            impl Index<&RegIndex> for I<'_> {
                type Output = Value;
                fn index(&self, index: &RegIndex) -> &Self::Output {
                    &self.0[self.1 + *index as usize]
                }
            }
            impl IndexMut<&RegIndex> for I<'_> {
                fn index_mut(&mut self, index: &RegIndex) -> &mut Self::Output {
                    let offset = self.1 + *index as usize;
                    if self.0.len() <= offset {
                        self.0.resize(offset + 1, Default::default())
                    }
                    &mut self.0[offset]
                }
            }
            let mut r = I(&mut self.registers, frame.base_pointer);
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
                        .map(|(symbol, i)| (*symbol, r[i]))
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
                        panic!("type error")
                    };
                    let Some(record) = addr.as_ref().downcast_ref::<Record>() else {
                        panic!("type error")
                    };
                    r[i] = record[symbol]
                }
                Instr::StoreField(i, symbol, j) => {
                    let value = r[j];
                    let Value::Dyn(addr) = &mut r[i] else {
                        panic!("type error")
                    };
                    let Some(record) = addr.as_mut().downcast_mut::<Record>() else {
                        panic!("type error")
                    };
                    if record.contains_key(&self.chunk_symbol) {
                        panic!("type error")
                    }
                    record.insert(*symbol, value);
                }
                Instr::Move(i, j) => r[j] = r[i],
                Instr::Operator(i, op, xs) => {
                    r[i] = if xs.len() == 2 {
                        match (op, r[&xs[0]], r[&xs[1]]) {
                            (Operator::Add, Value::I32(a), Value::I32(b)) => Value::I32(a + b),
                            (Operator::Sub, Value::I32(a), Value::I32(b)) => Value::I32(a - b),
                            (Operator::Mul, Value::I32(a), Value::I32(b)) => Value::I32(a * b),
                            (Operator::Div, Value::I32(a), Value::I32(b)) => Value::I32(a / b),
                            (Operator::Rem, Value::I32(a), Value::I32(b)) => Value::I32(a % b),
                            (Operator::Add, Value::Dyn(a), Value::Dyn(b)) => {
                                if let (Some(a), Some(b)) = (
                                    a.as_ref().downcast_ref::<String>(),
                                    b.as_ref().downcast_ref::<String>(),
                                ) {
                                    Value::Dyn(self.allocator.alloc([&**a, &**b].concat()))
                                } else {
                                    panic!("type error")
                                }
                            }
                            _ => panic!("type error"),
                        }
                    } else if xs.len() == 1 {
                        match (op, r[&xs[0]]) {
                            (Operator::Neg, Value::I32(a)) => Value::I32(-a),
                            _ => panic!("type error"),
                        }
                    } else {
                        unreachable!()
                    }
                }
                Instr::Apply(i, arity) => {
                    let Value::Dyn(addr) = r[i] else {
                        panic!("type error")
                    };
                    let Some(record) = addr.as_ref().downcast_ref::<Record>() else {
                        panic!("type error")
                    };
                    let Some(Value::ChunkIndex(chunk_index)) = record.get(&self.chunk_symbol)
                    else {
                        panic!("type error")
                    };
                    if *arity != self.chunks[*chunk_index].arity {
                        panic!("arity error")
                    }
                    let frame = Frame {
                        chunk_index: *chunk_index,
                        base_pointer: *i as _,
                        instr_pointer: 0,
                    };
                    self.frames.push(frame)
                }
                Instr::MatchField(i, symbol, instr) => {
                    let Value::Dyn(addr) = r[i] else {
                        panic!("type error")
                    };
                    let Some(record) = addr.as_ref().downcast_ref::<Record>() else {
                        panic!("type error")
                    };
                    if !record.contains_key(symbol) {
                        frame.instr_pointer = *instr
                    }
                }
                Instr::Jump(instr) => frame.instr_pointer = *instr,
            }
        }
    }
}
