use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
};

use petgraph::{
    graph::NodeIndex,
    visit::{EdgeRef, IntoNodeReferences},
    Direction::Incoming,
    Graph,
};

use crate::{
    ast::Operator,
    compile::{ChunkIndex, ConstIndex, Instr, InstrIndex, RecordTypeIndex, Symbol},
};

type ValueIndex = usize;

#[derive(Debug, Clone)]
pub struct Chunk {
    entry: NodeIndex,
    blocks: Graph<Block, ControlEdge>,
    // turn values into graph(s) as well?
    values: HashMap<ValueIndex, Vec<Value>>,
}

#[derive(Debug, Clone)]
struct Block {
    phis: HashMap<ValueIndex, usize>,
    effects: Vec<Effect>,
    exit: Exit,
}

#[derive(Debug, Clone)]
struct Value {
    op: Op,
    uses: Vec<ValueIndex>,
}

#[derive(Debug, Clone)]
enum Op {
    Unit,
    Param,
    Const(ConstIndex),
    Chunk(ChunkIndex),
    Copy,
    Phi,
    Record(RecordTypeIndex),
    Field(Symbol),
    Operator(Operator),
    Apply,
    Control,
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
    Jump,
    Match(ValueIndex, Symbol),
    Return(ValueIndex),
}

#[derive(Debug, Clone)]
enum ControlEdge {
    Jump,
    Match(bool),
}

impl Block {
    fn defines(&self) -> HashSet<ValueIndex> {
        let mut values = self.phis.keys().copied().collect::<HashSet<_>>();
        for instr in &self.effects {
            if let Effect::Define(v, _) = instr {
                values.insert(*v);
            }
        }
        values
    }
}

impl Chunk {
    pub fn new(instrs: &[Instr], arity: usize) -> Self {
        let mut block_offsets = BTreeSet::new();
        block_offsets.insert(0);
        for (i, instr) in instrs.iter().enumerate() {
            match instr {
                Instr::MatchField(_, _, index) => {
                    block_offsets.insert(*index);
                    block_offsets.insert(i + 1);
                }
                Instr::Jump(index) => {
                    block_offsets.insert(*index);
                    block_offsets.insert(i + 1);
                }
                _ => {}
            }
        }
        block_offsets.remove(&instrs.len());
        enum ExitControl {
            Return(ValueIndex),
            Match(ValueIndex, Symbol, InstrIndex, InstrIndex),
            Jump(InstrIndex),
        }

        let mut this = Self {
            entry: Default::default(),
            blocks: Default::default(),
            values: Default::default(),
        };
        // entry block, must have even if it's empty (when arity is 0)
        let mut block = Block {
            phis: Default::default(),
            effects: Default::default(),
            exit: Exit::Jump,
        };
        for v in 0..arity {
            block.effects.push(this.define_effect(
                v,
                Value {
                    op: Op::Param,
                    uses: Default::default(),
                },
            ));
        }
        this.entry = this.blocks.add_node(block);

        let mut block_indexes = HashMap::new();
        let mut block_exits = HashMap::new();
        block_exits.insert(this.entry, ExitControl::Jump(0));
        while let Some(offset) = block_offsets.pop_first() {
            let exit_index = block_offsets.first().copied().unwrap_or(instrs.len()) - 1;
            assert!(!(offset..=exit_index).is_empty());
            let mut end = exit_index;
            let exit = match &instrs[exit_index] {
                Instr::MatchField(r, symbol, index) => {
                    ExitControl::Match(*r as _, *symbol, exit_index + 1, *index)
                }
                Instr::Jump(index) => ExitControl::Jump(*index),
                Instr::Return(r) => ExitControl::Return(*r as _),
                _ => {
                    end += 1;
                    if exit_index + 1 == instrs.len() {
                        ExitControl::Return(0)
                    } else {
                        ExitControl::Jump(exit_index + 1)
                    }
                }
            };
            let effects = instrs[offset..end]
                .iter()
                .map(|instr| match instr {
                    Instr::LoadUnit(r) => this.define_effect(
                        *r as _,
                        Value {
                            op: Op::Unit,
                            uses: Default::default(),
                        },
                    ),
                    Instr::LoadConst(r, index) => this.define_effect(
                        *r as _,
                        Value {
                            op: Op::Const(*index),
                            uses: Default::default(),
                        },
                    ),
                    Instr::LoadChunk(r, index) => this.define_effect(
                        *r as _,
                        Value {
                            op: Op::Chunk(*index),
                            uses: Default::default(),
                        },
                    ),
                    Instr::LoadRecord(r, type_index, fields) => this.define_effect(
                        *r as _,
                        Value {
                            op: Op::Record(*type_index),
                            uses: fields.iter().map(|v| *v as _).collect(),
                        },
                    ),
                    Instr::LoadField(r, s, symbol) => this.define_effect(
                        *r as _,
                        Value {
                            op: Op::Field(*symbol),
                            uses: vec![*s as _],
                        },
                    ),
                    Instr::LoadCapture(..) | Instr::StoreCapture(..) => todo!(),
                    Instr::Copy(r, s) => this.define_effect(
                        *r as _,
                        Value {
                            op: Op::Copy,
                            uses: vec![*s as _],
                        },
                    ),
                    Instr::Operator(r, op, rs) => this.define_effect(
                        *r as _,
                        Value {
                            op: Op::Operator(*op),
                            uses: rs.iter().map(|r| *r as _).collect(),
                        },
                    ),
                    Instr::Apply(r, arity) => this.define_effect(
                        *r as _,
                        Value {
                            op: Op::Apply,
                            uses: (*r as usize..).take(*arity + 1).collect(),
                        },
                    ),
                    Instr::LoadControl(r) => this.define_effect(
                        *r as _,
                        Value {
                            op: Op::Control,
                            uses: Default::default(),
                        },
                    ),
                    Instr::StoreField(r, symbol, s) => {
                        Effect::StoreField(*r as _, *symbol, *s as _)
                    }
                    Instr::Spawn(r) => Effect::Spawn(*r as _),
                    Instr::Suspend(r) => Effect::Suspend(*r as _),
                    Instr::Resume(r) => Effect::Resume(*r as _),
                    Instr::MatchField(..) | Instr::Jump(_) | Instr::Return(_) => unreachable!(),
                })
                .collect::<Vec<_>>();
            let index = this.blocks.add_node(Block {
                phis: Default::default(),
                effects,
                exit: match exit {
                    ExitControl::Return(value_index) => Exit::Return(value_index),
                    ExitControl::Jump(_) => Exit::Jump,
                    ExitControl::Match(value_index, symbol, _, _) => {
                        Exit::Match(value_index, symbol)
                    }
                },
            });
            block_indexes.insert(offset, index);
            block_exits.insert(index, exit);
        }
        for (index, exit) in block_exits {
            match exit {
                ExitControl::Return(_) => {}
                ExitControl::Jump(offset) => {
                    this.blocks
                        .add_edge(index, block_indexes[&offset], ControlEdge::Jump);
                }
                ExitControl::Match(_, _, hit_offset, miss_offset) => {
                    this.blocks.add_edge(
                        index,
                        block_indexes[&hit_offset],
                        ControlEdge::Match(true),
                    );
                    this.blocks.add_edge(
                        index,
                        block_indexes[&miss_offset],
                        ControlEdge::Match(false),
                    );
                }
            }
        }
        this
    }

    fn define_effect(&mut self, value_index: ValueIndex, value: Value) -> Effect {
        let value_entry = self.values.entry(value_index).or_default();
        let version = value_entry.len();
        value_entry.push(value);
        Effect::Define(value_index, version)
    }

    #[allow(non_snake_case)]
    pub fn construct(mut self) -> Self {
        let dom = petgraph::algo::dominators::simple_fast(&self.blocks, self.entry);
        let mut DF = HashMap::<_, HashSet<_>>::new();
        for edge_index in self.blocks.edge_indices() {
            let Some((a, b)) = self.blocks.edge_endpoints(edge_index) else {
                unreachable!()
            };
            let mut x = a;
            while !dom
                .strict_dominators(b)
                .expect("entry has no engress edges")
                .any(|node_index| node_index == x)
            {
                DF.entry(x).or_default().insert(b);
                if let Some(node_index) = dom.immediate_dominator(x) {
                    x = node_index
                } else {
                    break;
                }
            }
        }
        // println!("{DF:?}");

        for (v, versions) in &mut self.values {
            let mut F = HashSet::<NodeIndex>::new();
            let mut W = self
                .blocks
                .node_references()
                .filter_map(|(b, block)| {
                    if block.defines().contains(v) {
                        Some(b)
                    } else {
                        None
                    }
                })
                .collect::<BTreeSet<_>>();
            while let Some(X) = W.pop_first() {
                // println!("{X:?} {:?}", DF[&X]);
                for Y in DF.get(&X).unwrap_or(&Default::default()) {
                    if !F.contains(Y) {
                        // TODO generalize to variable number of predecessors
                        assert_eq!(self.blocks.neighbors_directed(*Y, Incoming).count(), 2);
                        let version = versions.len();
                        versions.push(Value {
                            op: Op::Phi,
                            uses: vec![*v, *v],
                        });
                        self.blocks[*Y].phis.insert(*v, version);
                        F.insert(*Y);
                        if !self.blocks[*Y].defines().contains(v) {
                            W.insert(*Y);
                        }
                    }
                }
            }
        }

        let mut chunk = Chunk {
            entry: self.entry,
            blocks: self.blocks.map(
                |_, block| Block {
                    phis: Default::default(),
                    effects: Default::default(),
                    exit: block.exit.clone(),
                },
                |_, edge| edge.clone(),
            ),
            values: Default::default(),
        };

        let mut reaching_def = HashMap::<ValueIndex, ValueIndex>::new();
        let mut definition_indexes = HashMap::<ValueIndex, _>::new();
        let mut blocks = vec![self.entry];
        let mut defined_phis = HashSet::new();
        while let Some(BB) = blocks.pop() {
            // println!("{BB:?}");
            // println!("{:?}", dom.immediately_dominated_by(BB).collect::<Vec<_>>());
            blocks.extend(dom.immediately_dominated_by(BB).filter(|b| *b != BB));
            let update_reaching_def =
                |v,
                 reaching_def: &mut HashMap<usize, usize>,
                 definition_indexes: &HashMap<usize, _>| {
                    let mut r = reaching_def.get(&v);
                    while let Some(r1) = r {
                        if dom
                            .dominators(BB)
                            .unwrap()
                            .any(|b| b == definition_indexes[r1])
                        {
                            break;
                        }
                        r = reaching_def.get(r1)
                    }
                    if let Some(r) = r {
                        reaching_def.insert(v, *r);
                    } else {
                        reaching_def.remove(&v);
                    }
                };
            for (v, version) in &self.blocks[BB].phis {
                update_reaching_def(*v, &mut reaching_def, &definition_indexes);
                let v1 = chunk.define(self.values[v][*version].clone());
                chunk.blocks[BB].phis.insert(v1, 0);
                if let Some(r) = reaching_def.get(v) {
                    reaching_def.insert(v1, *r);
                }
                reaching_def.insert(*v, v1);
                definition_indexes.insert(v1, BB);
            }
            defined_phis.insert(BB);
            for mut effect in self.blocks[BB].effects.clone() {
                match &mut effect {
                    Effect::Define(v, version) => {
                        let mut value = self.values[v][*version].clone();
                        for use_v in &mut value.uses {
                            update_reaching_def(*use_v, &mut reaching_def, &definition_indexes);
                            *use_v = reaching_def[use_v]
                        }
                        update_reaching_def(*v, &mut reaching_def, &definition_indexes);
                        let v1 = chunk.define(value);
                        if let Some(r) = reaching_def.get(v) {
                            reaching_def.insert(v1, *r);
                        }
                        reaching_def.insert(*v, v1);
                        definition_indexes.insert(v1, BB);
                        effect = Effect::Define(v1, 0)
                    }
                    Effect::StoreField(record_v, _, v) => {
                        update_reaching_def(*record_v, &mut reaching_def, &definition_indexes);
                        *record_v = reaching_def[record_v];
                        update_reaching_def(*v, &mut reaching_def, &definition_indexes);
                        *v = reaching_def[v]
                    }
                    Effect::Spawn(v) | Effect::Suspend(v) | Effect::Resume(v) => {
                        update_reaching_def(*v, &mut reaching_def, &definition_indexes);
                        *v = reaching_def[v]
                    }
                }
                chunk.blocks[BB].effects.push(effect)
            }
            for b in self.blocks.neighbors(BB) {
                let index = self
                    .blocks
                    .neighbors_directed(b, Incoming)
                    // not sure whether iterator guarantee determinisic order but probably will be
                    // stable during program lifetime
                    // .collect::<BTreeSet<_>>()
                    // .into_iter()
                    .position(|index| index == BB)
                    .unwrap();
                // println!("{BB:?} -> {b:?} index {index}");
                let (phis, values) = if defined_phis.contains(&b) {
                    (&chunk.blocks[b].phis, &mut chunk.values)
                } else {
                    (&self.blocks[b].phis, &mut self.values)
                };
                for (v, version) in phis {
                    let value = &mut values.get_mut(v).unwrap()[*version];
                    // println!("{v}.{version} = {value:?}");
                    assert!(matches!(value.op, Op::Phi));
                    let use_v = &mut value.uses[index];
                    if self.blocks[BB].defines().contains(use_v) {
                        update_reaching_def(*use_v, &mut reaching_def, &definition_indexes);
                        *use_v = reaching_def[use_v]
                        // println!("{v}.{version} = {value:?}")
                    }
                }
            }
        }
        chunk
    }

    fn define(&mut self, value: Value) -> ValueIndex {
        let v = self.values.len();
        self.values.insert(v, vec![value]);
        v
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut line_break = false;
        for b in self.blocks.node_indices() {
            if line_break {
                writeln!(f)?
            }
            line_break = true;
            write!(f, "{b:?}")?;
            if b == self.entry {
                write!(f, " (entry)")?;
            }
            for (v, version) in &self.blocks[b].phis {
                writeln!(f)?;
                write!(f, "  {v}.{version} <= {:?}", self.values[v][*version])?
            }
            for effect in &self.blocks[b].effects {
                writeln!(f)?;
                if let Effect::Define(v, version) = effect {
                    write!(f, "  {v}.{version} <= {:?}", self.values[v][*version])?
                } else {
                    write!(f, "  {effect:?}")?
                }
            }
            writeln!(f)?;
            write!(f, "  {:?}", self.blocks[b].exit)?;
            for edge in self.blocks.edges(b) {
                writeln!(f)?;
                write!(f, "  -> {:?} {:?}", edge.target(), edge.weight())?
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // chunk for some func(x, y, _) where _ is branching indicator
    const SAMPLE_ARITY: usize = 3;

    fn sample() -> Vec<Instr> {
        vec![
            // A offset 0
            Instr::MatchField(2, Symbol::MAX, 4),
            // B offset 1
            Instr::LoadConst(0, 0), // consts[0] = Int(0)
            Instr::LoadConst(1, 0),
            Instr::Jump(8),
            // C offset 4
            Instr::Copy(3, 0), // tmp = alloc reg[3]
            Instr::Copy(0, 1),
            Instr::Copy(1, 3),
            Instr::MatchField(2, Symbol::MAX, 14),
            // D offset 8
            Instr::LoadChunk(3, 0), // f = alloc reg[3]
            Instr::Copy(4, 0),
            Instr::Copy(5, 1),
            Instr::Apply(3, 2),
            Instr::Copy(0, 3),
            Instr::MatchField(2, Symbol::MAX, 0),
            // E offset 14
            Instr::Copy(0, 0),
        ]
    }

    #[test]
    fn new_chunk() {
        let chunk = Chunk::new(&sample(), SAMPLE_ARITY);
        assert_eq!(chunk.blocks.node_count(), 5 + 1); // plus entry block
        assert_eq!(chunk.blocks.edge_count(), 7 + 1); // 3 * match + 1 * jump + entry
        assert_eq!(chunk.values.len(), 6)
    }

    #[test]
    fn construct() {
        let chunk = Chunk::new(&sample(), SAMPLE_ARITY);
        let chunk = chunk.construct();
        println!("{chunk}");
        assert!(chunk.values.values().all(|versions| versions.len() == 1))
    }
}
