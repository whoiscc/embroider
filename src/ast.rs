#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Unit,
    Integer(u64),
    String(String),
    Abstraction(Abstraction),

    Scoped((), Box<Expr>),
    Operator(Operator, Vec<Expr>),
    Apply(Box<Expr>, Vec<Expr>),

    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Abstraction {
    pub variables: Vec<String>,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scoped {
    decls: Vec<(String, Box<Expr>)>,
    stmts: Vec<Stmt>,
    value: Box<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,
    And,
    Or,
    Not,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Return(Box<Expr>),
    Break,
    Continue,
    While(Box<Expr>, Box<Expr>),
    MutEnv(String, Box<Expr>),
    MutField(Box<Expr>, String, Box<Expr>),
}
