pub type Symbol = usize;
pub type ConstIndex = usize;
pub type RegIndex = u8;
pub type InstrIndex = usize;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    LoadConst(RegIndex, ConstIndex),
    LoadSymbol(RegIndex, Symbol),
    LoadField(RegIndex, RegIndex, Symbol),

    StoreSymbol(RegIndex, Symbol),
    StoreField(RegIndex, Symbol, RegIndex),

    MatchField(RegIndex, Symbol, InstrIndex),
    Jump(InstrIndex),
}
