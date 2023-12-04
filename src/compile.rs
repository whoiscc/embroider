use std::{
    collections::HashMap,
    fmt::Display,
    mem::{replace, take},
};

use crate::ast::{Expr, Operator};

pub type Symbol = usize;
pub type ConstIndex = usize;
pub type RegIndex = u8;
pub type InstrIndex = usize;
pub type ChunkIndex = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    LoadUnit(RegIndex),
    LoadConst(RegIndex, ConstIndex),
    LoadField(RegIndex, RegIndex, Symbol),
    LoadRecord(RegIndex, Vec<(Symbol, RegIndex)>),
    LoadChunk(RegIndex, ChunkIndex),

    Move(RegIndex, RegIndex),
    Operator(RegIndex, Operator, Vec<RegIndex>),
    Apply(RegIndex, RegIndex, Vec<RegIndex>),

    StoreField(RegIndex, Symbol, RegIndex),

    MatchField(RegIndex, Symbol, InstrIndex),
    Jump(InstrIndex),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    Integer(u64),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Chunk {
    pub arity: usize,
    pub instrs: Vec<Instr>,
    pub consts: Vec<Const>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Compiler {
    pub chunks: Vec<Chunk>,
    symbols: Vec<String>,
    symbol_indexes: HashMap<String, Symbol>,

    instrs: Vec<Instr>,
    consts: Vec<Const>,
    reg_index: RegIndex,
    scopes: Vec<HashMap<String, RegIndex>>,
    quasi_scope: HashMap<String, RegIndex>,
    return_indexes: Vec<InstrIndex>,
    break_indexes: Vec<InstrIndex>,
    continue_jump_index: Option<InstrIndex>,
}

impl Compiler {
    fn intern(&mut self, symbol: String) -> Symbol {
        *self
            .symbol_indexes
            .entry(symbol.clone())
            .or_insert_with(|| {
                let s = self.symbols.len();
                self.symbols.push(symbol);
                s
            })
    }

    pub fn compile_chunk(&mut self, expr: Expr) -> anyhow::Result<ChunkIndex> {
        let saved_instrs = take(&mut self.instrs);
        let saved_consts = take(&mut self.consts);
        self.compile_expr(expr)?;
        let chunk_index = self.chunks.len();
        let chunk = Chunk {
            arity: 0,
            instrs: replace(&mut self.instrs, saved_instrs),
            consts: replace(&mut self.consts, saved_consts),
        };
        self.chunks.push(chunk);
        Ok(chunk_index)
    }

    fn compile_expr(&mut self, expr: Expr) -> anyhow::Result<()> {
        let reg_index = self.reg_index;
        match expr {
            Expr::Integer(integer) => {
                self.instrs
                    .push(Instr::LoadConst(reg_index, self.consts.len()));
                self.consts.push(Const::Integer(integer))
            }
            Expr::String(string) => {
                self.instrs
                    .push(Instr::LoadConst(reg_index, self.consts.len()));
                self.consts.push(Const::String(string))
            }
            Expr::Record(rows) => {
                let mut operands = Vec::new();
                for (name, expr) in rows {
                    self.reg_index += 1;
                    self.compile_expr(*expr)?;
                    let symbol = self.intern(name);
                    operands.push((symbol, self.reg_index));
                }
                self.instrs.push(Instr::LoadRecord(reg_index, operands))
            }
            Expr::Abstraction(abstraction) => {
                let arity = abstraction.variables.len();
                let saved_scopes = take(&mut self.scopes); // TODO
                self.reg_index = 0;
                let mut scope = HashMap::new();
                for name in abstraction.variables {
                    scope.insert(name, self.reg_index);
                    self.reg_index += 1
                }
                self.scopes.push(scope);
                let saved_return_indexes = take(&mut self.return_indexes);
                let saved_instrs = take(&mut self.instrs);
                let saved_consts = take(&mut self.consts);
                self.compile_expr(*abstraction.expr)?;
                let return_jump_index = self.instrs.len();
                for return_index in replace(&mut self.return_indexes, saved_return_indexes) {
                    let Instr::Jump(index) = &mut self.instrs[return_index] else {
                        unreachable!()
                    };
                    *index = return_jump_index
                }
                self.scopes = saved_scopes;
                let chunk_index = self.chunks.len();
                let chunk = Chunk {
                    arity,
                    instrs: replace(&mut self.instrs, saved_instrs),
                    consts: replace(&mut self.consts, saved_consts),
                };
                self.chunks.push(chunk);
                self.instrs.push(Instr::LoadChunk(reg_index, chunk_index))
            }
            Expr::Variable(name) => {
                let mut found = None;
                for scope in self.scopes.iter().rev() {
                    if let Some(reg_index) = scope.get(&name) {
                        found = Some(*reg_index);
                        break;
                    }
                }
                self.instrs.push(Instr::Move(
                    self.reg_index,
                    found.ok_or(anyhow::anyhow!("variable {name} is not defined"))?,
                ))
            }
            Expr::GetField(expr, name) => {
                self.reg_index += 1;
                self.compile_expr(*expr)?;
                let symbol = self.intern(name);
                self.instrs
                    .push(Instr::LoadField(reg_index, self.reg_index, symbol))
            }
            Expr::Scoped(scoped) => {
                if !self.quasi_scope.is_empty() {
                    anyhow::bail!("nested `with` is not allowed")
                }
                for (i, (name, _)) in scoped.decls.iter().enumerate() {
                    self.quasi_scope
                        .insert(name.clone(), self.reg_index + i as RegIndex);
                }
                for (_, expr) in scoped.decls {
                    self.compile_expr(expr)?;
                    self.reg_index += 1
                }
                self.scopes.push(take(&mut self.quasi_scope));
                for expr in scoped.exprs {
                    self.compile_expr(expr)?
                }
                if let Some(expr) = scoped.value_expr {
                    self.compile_expr(*expr)?
                } else {
                    self.instrs.push(Instr::LoadUnit(self.reg_index))
                }
                self.instrs.push(Instr::Move(reg_index, self.reg_index))
            }
            Expr::Operator(op, exprs) => {
                for expr in exprs {
                    self.compile_expr(expr)?;
                    self.reg_index += 1
                }
                self.instrs.push(Instr::Operator(
                    reg_index,
                    op,
                    (reg_index..self.reg_index).collect(),
                ))
            }
            Expr::Apply(abstraction, exprs) => {
                self.compile_expr(*abstraction)?;
                for expr in exprs {
                    self.reg_index += 1;
                    self.compile_expr(expr)?
                }
                self.instrs.push(Instr::Apply(
                    reg_index,
                    reg_index,
                    (reg_index..=self.reg_index).collect(),
                ))
            }
            Expr::Mut(name, expr) => {
                self.compile_expr(*expr)?;
                let mut found = None;
                for scope in self.scopes.iter().rev() {
                    if let Some(reg_index) = scope.get(&name) {
                        found = Some(*reg_index);
                        break;
                    }
                }
                self.instrs.push(Instr::Move(
                    found.ok_or(anyhow::anyhow!("variable {name} is not defined"))?,
                    reg_index,
                ));
                self.instrs.push(Instr::LoadUnit(reg_index))
            }
            Expr::MutField(record, name, expr) => {
                self.compile_expr(*record)?;
                self.reg_index += 1;
                self.compile_expr(*expr)?;
                let symbol = self.intern(name);
                self.instrs
                    .push(Instr::StoreField(reg_index, symbol, self.reg_index));
                self.instrs.push(Instr::LoadUnit(reg_index))
            }
            Expr::Match(matching) => {
                self.compile_expr(*matching.variant)?;
                let mut converge_indexes = Vec::new();
                for (tag, name, expr) in matching.cases {
                    let symbol = self.intern(tag);
                    let match_index = self.instrs.len();
                    self.instrs
                        .push(Instr::MatchField(reg_index, symbol, InstrIndex::MAX));
                    let mut scope = HashMap::new();
                    if name != "*" {
                        self.instrs
                            .push(Instr::LoadField(reg_index + 1, reg_index, symbol));
                        scope.insert(name, reg_index + 1);
                    }
                    self.reg_index = reg_index + 2;
                    self.scopes.push(scope);
                    self.compile_expr(expr)?;
                    self.scopes.pop().unwrap();
                    let match_jump_index = self.instrs.len();
                    let Instr::MatchField(_, _, index) = &mut self.instrs[match_index] else {
                        unreachable!()
                    };
                    *index = match_jump_index;
                    converge_indexes.push(self.instrs.len());
                    self.instrs.push(Instr::Jump(InstrIndex::MAX))
                }
                let converge_jump_index = self.instrs.len();
                for index in converge_indexes {
                    let Instr::Jump(index) = &mut self.instrs[index] else {
                        unreachable!()
                    };
                    *index = converge_jump_index
                }
            }
            Expr::Loop(expr) => {
                let saved_break_indexes = take(&mut self.break_indexes);
                let saved_continue_jump_index =
                    replace(&mut self.continue_jump_index, Some(self.instrs.len()));
                self.compile_expr(*expr)?;
                self.instrs
                    .push(Instr::Jump(self.continue_jump_index.unwrap()));
                let break_jump_index = self.instrs.len();
                for break_index in replace(&mut self.break_indexes, saved_break_indexes) {
                    let Instr::Jump(index) = &mut self.instrs[break_index] else {
                        unreachable!()
                    };
                    *index = break_jump_index
                }
                self.continue_jump_index = saved_continue_jump_index
            }
            Expr::Return(expr) => {
                self.compile_expr(*expr)?;
                self.instrs.push(Instr::Move(0, reg_index));
                self.return_indexes.push(self.instrs.len());
                self.instrs.push(Instr::Jump(InstrIndex::MAX))
            }
            Expr::Break => {
                self.break_indexes.push(self.instrs.len());
                self.instrs.push(Instr::Jump(InstrIndex::MAX))
            }
            Expr::Continue => self.instrs.push(Instr::Jump(
                self.continue_jump_index
                    .ok_or(anyhow::anyhow!("continue from outside loop"))?,
            )),
        }
        self.reg_index = reg_index;
        Ok(())
    }

    pub fn disassemble(&self, index: ChunkIndex) -> DisassembleChunk<'_> {
        DisassembleChunk {
            compiler: self,
            chunk: &self.chunks[index],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DisassembleChunk<'a> {
    compiler: &'a Compiler,
    chunk: &'a Chunk,
}

impl Display for DisassembleChunk<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (index, instr) in self.chunk.instrs.iter().enumerate() {
            write!(f, "{index:>4} {instr:?}")?;
            if index != self.chunk.instrs.len() - 1 {
                writeln!(f)?
            }
        }
        Ok(())
    }
}
