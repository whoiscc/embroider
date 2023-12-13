use std::{
    collections::{HashMap, HashSet},
    error::Error,
    fmt::Display,
    mem::{replace, take},
};

use crate::ast::{Expr, ExprO, Operator};

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
    Apply(RegIndex, usize),

    StoreField(RegIndex, Symbol, RegIndex),

    MatchField(RegIndex, Symbol, InstrIndex),
    Jump(InstrIndex),

    Spawn(RegIndex),
    LoadControl(RegIndex),
    Suspend(RegIndex),
    Resume(RegIndex),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    I32(i32),
    String(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Chunk {
    pub description: String,
    pub arity: usize,
    pub instrs: Vec<Instr>,
    pub consts: Vec<Const>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Compiler {
    pub chunks: Vec<Chunk>,
    pub lang_indexes: HashMap<String, ChunkIndex>,
    pub symbols: Vec<String>,
    symbol_indexes: HashMap<String, Symbol>,
    modules: HashMap<String, ChunkIndex>,
    module_placeholders: HashMap<String, ChunkIndex>,

    instrs: Vec<Instr>,
    consts: Vec<Const>,
    reg_index: RegIndex,
    scopes: Vec<HashMap<String, RegIndex>>,
    capture_scopes: Vec<HashMap<String, RegIndex>>,
    captures: HashSet<String>,
    forward_captures: HashMap<String, Vec<RegIndex>>,
    return_indexes: Vec<InstrIndex>,
    break_indexes: Vec<InstrIndex>,
    continue_jump_index: Option<InstrIndex>,
    description_hint: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompileError(pub CompileErrorKind, pub usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompileErrorKind {
    ResolveFail(String),
    NonCaptureResolveFail(String),
    RegisterExhausted,
    ContinueOutsideLoop,
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Error for CompileError {}

impl Compiler {
    pub fn intern(&mut self, symbol: String) -> Symbol {
        *self
            .symbol_indexes
            .entry(symbol.clone())
            .or_insert_with(|| {
                let s = self.symbols.len();
                self.symbols.push(symbol);
                s
            })
    }

    pub fn compile_module(&mut self, name: String, expr: ExprO) -> anyhow::Result<ChunkIndex> {
        let saved_instrs = take(&mut self.instrs);
        let saved_consts = take(&mut self.consts);
        self.compile_expr(expr)?;
        let chunk = Chunk {
            description: name.clone(),
            arity: 0,
            instrs: replace(&mut self.instrs, saved_instrs),
            consts: replace(&mut self.consts, saved_consts),
        };
        let chunk_index = if let Some(chunk_index) = self.module_placeholders.remove(&name) {
            self.chunks[chunk_index] = chunk;
            chunk_index
        } else {
            let chunk_index = self.chunks.len();
            self.chunks.push(chunk);
            chunk_index
        };
        self.modules.insert(name, chunk_index);
        Ok(chunk_index)
    }

    pub fn next_imported(&self) -> Option<&str> {
        self.module_placeholders
            .keys()
            .map(std::ops::Deref::deref)
            .next()
    }

    pub fn resolve(&mut self, name: String, capturing: bool) -> Result<RegIndex, CompileErrorKind> {
        for scope in self.scopes.iter().rev() {
            if let Some(reg_index) = scope.get(&name) {
                return Ok(*reg_index);
            }
        }
        if !capturing {
            return Err(CompileErrorKind::NonCaptureResolveFail(name));
        }
        let symbol = self.intern(name.clone());
        for scope in self.capture_scopes.iter().rev() {
            if scope.get(&name).is_some() {
                self.captures.insert(name.clone());
                // is it always safe to directly use slot `self.reg_index`?
                self.instrs
                    .push(Instr::LoadField(self.reg_index, 0, symbol));
                return Ok(self.reg_index);
            }
        }
        Err(CompileErrorKind::ResolveFail(name))
    }

    // convention: anything below `self.reg_index` is unchanged
    // `self.reg_index` is updated with expr's evaluated value
    // anything above `self.ref_index` is ok to be overwritten
    // return, break and continue does not follow this convention
    fn compile_expr(&mut self, expr: ExprO) -> anyhow::Result<()> {
        let reg_index = self.reg_index;
        if reg_index == RegIndex::MAX {
            Err(CompileError(CompileErrorKind::RegisterExhausted, expr.1))?
        }
        let offset = expr.1;
        match expr.0 {
            Expr::Integer(integer) => {
                self.instrs
                    .push(Instr::LoadConst(reg_index, self.consts.len()));
                self.consts.push(Const::I32(integer))
            }
            Expr::String(string) => {
                self.instrs
                    .push(Instr::LoadConst(reg_index, self.consts.len()));
                self.consts.push(Const::String(string))
            }
            Expr::Record(rows) => {
                let mut operands = Vec::new();
                let saved_description_hint = take(&mut self.description_hint);
                for (name, expr) in rows {
                    self.reg_index += 1;
                    self.description_hint = name.clone();
                    self.compile_expr(expr)?;
                    let symbol = self.intern(name);
                    operands.push((symbol, self.reg_index));
                }
                self.description_hint = saved_description_hint;
                self.instrs.push(Instr::LoadRecord(reg_index, operands))
            }
            Expr::Abstraction(abstraction) => {
                let arity = abstraction.variables.len();
                let saved_captures = take(&mut self.captures);
                let saved_scopes = take(&mut self.scopes);
                let saved_capture_scopes = self.capture_scopes.clone();
                self.capture_scopes.extend(saved_scopes.clone());
                let mut forward_scope = HashMap::new();
                for name in abstraction.captures {
                    forward_scope.insert(name.clone(), RegIndex::MAX);
                    self.forward_captures
                        .entry(name)
                        .or_default()
                        .push(reg_index)
                }
                self.capture_scopes.push(forward_scope.clone());
                let mut scope = HashMap::new();
                let mut i = 1; // reserve register 0 for the "closure object"
                for name in abstraction.variables {
                    scope.insert(name, i);
                    i += 1
                }
                self.scopes.push(scope);
                let saved_return_indexes = take(&mut self.return_indexes);
                let saved_continue_jump_index = take(&mut self.continue_jump_index);
                let saved_break_indexes = take(&mut self.break_indexes);
                let saved_instrs = take(&mut self.instrs);
                let saved_consts = take(&mut self.consts);
                self.reg_index = i;
                // println!("{:?}", self.capture_scopes);
                self.compile_expr(*abstraction.expr)?;
                self.instrs.push(Instr::Move(0, i));
                let return_jump_index = self.instrs.len();
                for return_index in replace(&mut self.return_indexes, saved_return_indexes) {
                    let Instr::Jump(index) = &mut self.instrs[return_index] else {
                        unreachable!()
                    };
                    *index = return_jump_index
                }
                self.continue_jump_index = saved_continue_jump_index;
                self.break_indexes = saved_break_indexes;
                let chunk_index = self.chunks.len();
                let chunk = Chunk {
                    description: self.description_hint.clone(),
                    arity,
                    instrs: replace(&mut self.instrs, saved_instrs),
                    consts: replace(&mut self.consts, saved_consts),
                };
                self.chunks.push(chunk);
                if let Some(lang) = abstraction.lang {
                    self.lang_indexes.insert(lang, chunk_index);
                }
                self.instrs.push(Instr::LoadChunk(reg_index, chunk_index));
                self.scopes = saved_scopes;
                self.capture_scopes = saved_capture_scopes;
                // `resolve` which may use `self.reg_index`, so adjust it to a safe position
                self.reg_index = reg_index + 1;
                // println!("{} {:?}", chunk_index, self.quasi_scope);
                // println!("{:?}", self.captures);
                for name in replace(&mut self.captures, saved_captures) {
                    if forward_scope.contains_key(&name) {
                        continue;
                    }
                    let symbol = self.intern(name.clone());
                    let resolved = self
                        .resolve(name.clone(), true)
                        .unwrap_or_else(|kind| panic!("{}", CompileError(kind, offset)));
                    self.instrs
                        .push(Instr::StoreField(reg_index, symbol, resolved))
                }
            }
            Expr::Variable(name) => {
                let resolved = self
                    .resolve(name.clone(), true)
                    .map_err(|kind| CompileError(kind, offset))?;
                self.instrs.push(Instr::Move(self.reg_index, resolved))
            }
            Expr::GetField(expr, name) => {
                self.reg_index += 1;
                self.compile_expr(*expr)?;
                let symbol = self.intern(name);
                self.instrs
                    .push(Instr::LoadField(reg_index, self.reg_index, symbol))
            }
            Expr::Scoped(scoped) => {
                let saved_scopes = self.scopes.clone();
                let saved_forward_captures = take(&mut self.forward_captures);
                let saved_description_hint = take(&mut self.description_hint);
                self.scopes.push(Default::default());
                for (name, expr) in scoped.decls {
                    self.description_hint = name.clone();
                    // println!("before {name}: {:?}", self.quasi_scope);
                    self.compile_expr(expr)?;
                    let symbol = self.intern(name.clone());
                    for reg_index in self.forward_captures.remove(&name).unwrap_or_default() {
                        self.instrs
                            .push(Instr::StoreField(reg_index, symbol, reg_index))
                    }
                    self.scopes.last_mut().unwrap().insert(name, self.reg_index);
                    self.reg_index += 1
                }
                self.description_hint = saved_description_hint;
                for expr in scoped.exprs {
                    self.compile_expr(expr)?
                }
                if let Some(expr) = scoped.value_expr {
                    self.compile_expr(*expr)?
                } else {
                    self.instrs.push(Instr::LoadUnit(self.reg_index))
                }
                self.scopes = saved_scopes;
                self.forward_captures = saved_forward_captures;
                self.instrs.push(Instr::Move(reg_index, self.reg_index))
            }
            Expr::Operator(op, exprs) => {
                // TODO and or
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
                let arity = exprs.len();
                for expr in exprs {
                    self.reg_index += 1;
                    self.compile_expr(expr)?
                }
                self.instrs.push(Instr::Apply(reg_index, arity))
            }
            Expr::Mut(name, expr) => {
                self.compile_expr(*expr)?;
                let resolved = self
                    .resolve(name, false)
                    .map_err(|kind| CompileError(kind, offset))?;
                self.instrs.push(Instr::Move(resolved, reg_index));
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
            Expr::Capture(name, variable) => {
                let resolved = self
                    .resolve(name, false)
                    .map_err(|kind| CompileError(kind, offset))?;
                let resolved_variable = self
                    .resolve(variable.clone(), false)
                    .map_err(|kind| CompileError(kind, offset))?;
                let symbol = self.intern(variable);
                self.instrs
                    .push(Instr::StoreField(resolved, symbol, resolved_variable))
            }
            Expr::Match(matching) => {
                self.compile_expr(*matching.variant)?;
                let mut converge_indexes = Vec::new();
                self.reg_index = reg_index + 2;
                for (tag, name, expr) in matching.cases {
                    let symbol = self.intern(tag);
                    let match_index = self.instrs.len();
                    self.instrs
                        .push(Instr::MatchField(reg_index, symbol, InstrIndex::MAX));
                    let mut scope = HashMap::new();
                    if let Some(name) = name {
                        self.instrs
                            .push(Instr::LoadField(reg_index + 1, reg_index, symbol));
                        scope.insert(name, reg_index + 1);
                    }
                    self.scopes.push(scope);
                    self.compile_expr(expr)?;
                    converge_indexes.push(self.instrs.len());
                    self.instrs.push(Instr::Jump(InstrIndex::MAX));
                    self.scopes.pop().unwrap();
                    let match_jump_index = self.instrs.len();
                    let Instr::MatchField(_, _, index) = &mut self.instrs[match_index] else {
                        unreachable!()
                    };
                    *index = match_jump_index
                }
                // all matching failed: evaluate to unit
                self.instrs.push(Instr::LoadUnit(self.reg_index));
                let converge_jump_index = self.instrs.len();
                for index in converge_indexes {
                    let Instr::Jump(index) = &mut self.instrs[index] else {
                        unreachable!()
                    };
                    *index = converge_jump_index
                }
                self.instrs.push(Instr::Move(reg_index, self.reg_index))
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
            Expr::Continue => self
                .instrs
                .push(Instr::Jump(self.continue_jump_index.ok_or(
                    CompileError(CompileErrorKind::ContinueOutsideLoop, offset),
                )?)),
            Expr::Spawn(expr) => {
                self.compile_expr(*expr)?;
                self.instrs.push(Instr::Spawn(reg_index));
                self.instrs.push(Instr::LoadUnit(reg_index))
            }
            Expr::Control => self.instrs.push(Instr::LoadControl(reg_index)),
            Expr::Suspend(expr) => {
                self.compile_expr(*expr)?;
                self.instrs.push(Instr::Suspend(reg_index));
                self.instrs.push(Instr::LoadUnit(reg_index))
            }
            Expr::Resume(expr) => {
                self.compile_expr(*expr)?;
                self.instrs.push(Instr::Resume(reg_index));
                self.instrs.push(Instr::LoadUnit(reg_index))
            }
            Expr::Import(name) => {
                let chunk_index = if let Some(chunk_index) = self.modules.get(&name) {
                    *chunk_index
                } else {
                    let chunk_index = self.chunks.len();
                    self.module_placeholders.insert(name, chunk_index);
                    self.chunks.push(Chunk::default());
                    chunk_index
                };
                self.instrs.push(Instr::LoadChunk(reg_index, chunk_index));
                self.instrs.push(Instr::Apply(reg_index, 0))
            }
        }
        self.reg_index = reg_index;
        Ok(())
    }

    pub fn disassemble(&self, index: ChunkIndex) -> DisassembleChunk<'_> {
        DisassembleChunk {
            compiler: self,
            chunk_index: index,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DisassembleChunk<'a> {
    compiler: &'a Compiler,
    chunk_index: usize,
}

impl Display for DisassembleChunk<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let chunk = &self.compiler.chunks[self.chunk_index];
        writeln!(f, "chunk {} {}", self.chunk_index, chunk.description)?;
        for (index, instr) in chunk.instrs.iter().enumerate() {
            write!(f, "  {index:>4} {instr:?}")?;
            match instr {
                Instr::LoadConst(_, index) => write!(f, " {:?}", chunk.consts[*index])?,
                Instr::LoadChunk(_, index) => {
                    write!(f, " {}", self.compiler.chunks[*index].description)?
                }
                Instr::LoadField(_, _, symbol) => write!(f, " {}", self.compiler.symbols[*symbol])?,
                Instr::StoreField(_, symbol, _) => {
                    write!(f, " {}", self.compiler.symbols[*symbol])?
                }
                Instr::MatchField(_, symbol, _) => {
                    write!(f, " {}", self.compiler.symbols[*symbol])?
                }
                Instr::LoadRecord(_, rows) => write!(
                    f,
                    " {}",
                    rows.iter()
                        .map(|(symbol, _)| &*self.compiler.symbols[*symbol])
                        .collect::<Vec<_>>()
                        .join(" ")
                )?,
                _ => {}
            }
            if index != chunk.instrs.len() - 1 {
                writeln!(f)?
            }
        }
        Ok(())
    }
}
