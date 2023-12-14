use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{
    ast::Operator,
    compile::{ChunkIndex, ConstIndex, Instr, Symbol},
};

type BlockIndex = usize;
type ValueIndex = usize;

#[derive(Debug, Clone)]
pub struct Chunk {
    blocks: HashMap<BlockIndex, Block>,
    values: HashMap<ValueIndex, Vec<Value>>,
}

#[derive(Debug, Clone)]
struct Block {
    predecessors: Vec<BlockIndex>,
    phis: HashSet<ValueIndex>,
    effects: Vec<Effect>,
    exit: Exit,
}

#[derive(Debug, Clone)]
enum Value {
    Param,
    Unit,
    Const(ConstIndex),
    Field(ValueIndex, Symbol),
    Record(Vec<(Symbol, ValueIndex)>),
    Chunk(ChunkIndex),
    Copy(ValueIndex),
    Operator(Operator, Vec<ValueIndex>),
    Apply(ValueIndex, Vec<ValueIndex>),
    Phi(Vec<ValueIndex>),
}

impl Value {
    const CONTROL: ValueIndex = ValueIndex::MAX - 1;
}

#[derive(Debug, Clone)]
enum Effect {
    Define(ValueIndex, usize),
    StoreField(ValueIndex, Symbol, ValueIndex),
    Spawn(ValueIndex),
    Suspend(ValueIndex),
    Resume(ValueIndex),
}

#[derive(Debug, Clone)]
enum Exit {
    Jump(BlockIndex),
    Match(ValueIndex, Symbol, BlockIndex, BlockIndex),
    Return(ValueIndex),
}

impl Value {
    fn uses(&self) -> HashSet<ValueIndex> {
        match self {
            Self::Param | Self::Unit | Self::Chunk(_) | Self::Const(_) => Default::default(),
            Self::Copy(value) | Self::Field(value, _) => [*value].into_iter().collect(),
            Self::Operator(_, values) | Self::Phi(values) => values.iter().copied().collect(),
            Self::Record(rows) => rows.iter().map(|(_, value)| *value).collect(),
            Self::Apply(abstraction, arguments) => {
                let mut values = arguments.iter().copied().collect::<HashSet<_>>();
                values.insert(*abstraction);
                values
            }
        }
    }
}

impl Effect {
    fn uses(&self) -> HashSet<ValueIndex> {
        match self {
            Self::Spawn(value)
            | Self::Suspend(value)
            | Self::Resume(value)
            | Self::Define(value, _) => [*value].into_iter().collect(),
            Self::StoreField(record, _, field) => [*record, *field].into_iter().collect(),
        }
    }
}

impl Exit {
    fn uses(&self) -> HashSet<ValueIndex> {
        match self {
            Self::Jump(_) => Default::default(),
            Self::Match(v, ..) | Self::Return(v) => [*v].into_iter().collect(),
        }
    }
}

impl Block {
    fn uses(&self) -> HashSet<ValueIndex> {
        let mut values = self.phis.clone();
        for instr in &self.effects {
            values.extend(instr.uses())
        }
        values.extend(self.exit.uses());
        values
    }
}

impl Chunk {
    pub fn new(instrs: &[Instr]) -> Self {
        let mut block_offsets = BTreeMap::new();
        block_offsets.insert(0, Vec::new());
        let mut block_offset = 0;
        for (i, instr) in instrs.iter().enumerate() {
            match instr {
                Instr::MatchField(_, _, index) => {
                    block_offsets.insert(i + 1, vec![block_offset]);
                    block_offsets.entry(*index).or_default().push(block_offset);
                    block_offset = i + 1
                }
                Instr::Jump(index) => {
                    block_offsets.insert(i + 1, Vec::new());
                    block_offsets.entry(*index).or_default().push(block_offset);
                    block_offset = i + 1
                }
                _ => {}
            }
        }
        let mut returning_block_offsets = block_offsets.remove(&instrs.len()).unwrap_or_default();
        if block_offset < instrs.len() {
            returning_block_offsets.push(block_offset)
        }
        let block_indexes = block_offsets
            .keys()
            .enumerate()
            .map(|(i, offset)| (*offset, i))
            .collect::<HashMap<_, _>>();

        let mut this = Self {
            blocks: Default::default(),
            values: Default::default(),
        };
        let mut block_index = 0;
        while let Some((offset, predecessor_offsets)) = block_offsets.pop_first() {
            let predecessors = predecessor_offsets
                .into_iter()
                .map(|offset| block_indexes[&offset])
                .collect();
            let exit_index = block_offsets.keys().copied().next().unwrap_or(instrs.len()) - 1;
            assert!(!(offset..=exit_index).is_empty());
            let mut end = exit_index;
            let exit = match &instrs[exit_index] {
                Instr::MatchField(r, symbol, index) => Exit::Match(
                    *r as _,
                    *symbol,
                    block_indexes[&(exit_index + 1)],
                    block_indexes[index],
                ),
                Instr::Jump(index) => {
                    if *index == instrs.len() {
                        Exit::Return(0)
                    } else {
                        Exit::Jump(block_indexes[index])
                    }
                }
                _ => {
                    end += 1;
                    if exit_index + 1 == instrs.len() {
                        Exit::Return(0)
                    } else {
                        Exit::Jump(block_indexes[&(exit_index + 1)])
                    }
                }
            };
            let effects = instrs[offset..end]
                .iter()
                .map(|instr| match instr {
                    Instr::LoadUnit(r) => this.define(*r as _, Value::Unit),
                    Instr::LoadConst(r, index) => this.define(*r as _, Value::Const(*index)),
                    Instr::LoadChunk(r, index) => this.define(*r as _, Value::Chunk(*index)),
                    Instr::LoadRecord(r, rows) => this.define(
                        *r as _,
                        Value::Record(
                            rows.iter()
                                .map(|(symbol, value)| (*symbol, *value as _))
                                .collect(),
                        ),
                    ),
                    Instr::LoadField(r, s, symbol) => {
                        this.define(*r as _, Value::Field(*s as _, *symbol))
                    }
                    Instr::Copy(r, s) => this.define(*r as _, Value::Copy(*s as _)),
                    Instr::Operator(r, op, rs) => this.define(
                        *r as _,
                        Value::Operator(*op, rs.iter().map(|r| *r as _).collect()),
                    ),
                    Instr::Apply(r, arity) => this.define(
                        *r as _,
                        Value::Apply(*r as _, (*r as usize + 1..).take(*arity).collect()),
                    ),
                    Instr::LoadControl(r) => {
                        this.define(*r as _, Value::Apply(Value::CONTROL, Default::default()))
                    }
                    Instr::StoreField(r, symbol, s) => {
                        Effect::StoreField(*r as _, *symbol, *s as _)
                    }
                    Instr::Spawn(r) => Effect::Spawn(*r as _),
                    Instr::Suspend(r) => Effect::Suspend(*r as _),
                    Instr::Resume(r) => Effect::Resume(*r as _),
                    Instr::MatchField(..) | Instr::Jump(_) => unreachable!(),
                })
                .collect();
            this.blocks.insert(
                block_index,
                Block {
                    predecessors,
                    phis: Default::default(),
                    effects,
                    exit,
                },
            );
            block_index += 1
        }
        this
    }

    fn define(&mut self, value_index: ValueIndex, value: Value) -> Effect {
        let value_entry = self.values.entry(value_index).or_default();
        let version = value_entry.len();
        value_entry.push(value);
        Effect::Define(value_index, version)
    }
}
