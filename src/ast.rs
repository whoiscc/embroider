pub type ExprO = (Expr, usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Integer(i32),
    String(String),
    Record(Vec<(String, ExprO)>),
    Abstraction(Abstraction),

    Variable(String),

    GetField(Box<ExprO>, String),
    Scoped(Scoped),
    Operator(Operator, Vec<ExprO>),
    Apply(Box<ExprO>, Vec<ExprO>),

    Mut(String, Box<ExprO>),
    MutField(Box<ExprO>, String, Box<ExprO>),

    Return(Box<ExprO>),
    Break,
    Continue,

    Match(Match),
    Loop(Box<ExprO>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Abstraction {
    pub variables: Vec<String>,
    pub expr: Box<ExprO>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scoped {
    pub decls: Vec<(String, ExprO)>,
    pub exprs: Vec<ExprO>,
    pub value_expr: Option<Box<ExprO>>,
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
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Match {
    pub variant: Box<ExprO>,
    pub cases: Vec<(String, Option<String>, ExprO)>,
}

peg::parser! {
    pub grammar parse() for str {
        pub rule boxed_expr() -> Box<ExprO> = e:expr() { Box::new(e) }

        pub rule expr() -> ExprO = precedence! {
            p:position!() e:@ _:position!() { (e, p) }
            --
            e:@ _ "mut." v:variable() _ m:boxed_expr() { Expr::MutField(Box::new(e), v, m) }
            v:variable() _ "mut" _ m:boxed_expr() { Expr::Mut(v, m) }
            --
            e:(@) _ "or" _ x:@ { Expr::Operator(Operator::Or, vec![e, x]) }
            --
            e:(@) _ "and" _ x:@ { Expr::Operator(Operator::And, vec![e, x]) }
            --
            e:(@) _ "+" _ x:@ { Expr::Operator(Operator::Add, vec![e, x]) }
            e:(@) _ "-" _ x:@ { Expr::Operator(Operator::Sub, vec![e, x]) }
            --
            e:(@) _ "*" _ x:@ { Expr::Operator(Operator::Mul, vec![e, x]) }
            e:(@) _ "/" _ x:@ { Expr::Operator(Operator::Div, vec![e, x]) }
            e:(@) _ "%" _ x:@ { Expr::Operator(Operator::Rem, vec![e, x]) }
            --
            e:(@) _ "==" _ x:@ { Expr::Operator(Operator::Eq, vec![e, x]) }
            e:(@) _ "!=" _ x:@ { Expr::Operator(Operator::Ne, vec![e, x]) }
            e:(@) _ "<=" _ x:@ { Expr::Operator(Operator::Le, vec![e, x]) }
            e:(@) _ ">=" _ x:@ { Expr::Operator(Operator::Ge, vec![e, x]) }
            e:(@) _ "<" _ x:@ { Expr::Operator(Operator::Lt, vec![e, x]) }
            e:(@) _ ">" _ x:@ { Expr::Operator(Operator::Gt, vec![e, x]) }
            --
            "-" _ e:@ { Expr::Operator(Operator::Neg, vec![e]) }
            --
            e:@ "." v:variable() { Expr::GetField(Box::new(e), v) }
            e:@ "(" _ es:optional_delimited(<expr()>, <",">) _ ")" { Expr::Apply(Box::new(e), es) }
            --
            e:integer() { Expr::Integer(e) }
            e:string() { Expr::String(e) }
            e:abstraction() { Expr::Abstraction(e) }
            e:scoped() { Expr::Scoped(e) }
            e:record() { Expr::Record(e) }
            e:match() { Expr::Match(e) }
            "loop" _ e:boxed_expr() { Expr::Loop(e) }
            "return" _ e:boxed_expr() { Expr::Return(e) }
            "break" { Expr::Break }
            "continue" { Expr::Continue }
            e:variable() { Expr::Variable(e) }
        }

        rule _ = quiet!{[' ' | '\n']*}

        rule optional_delimited<T>(r: rule<T>, delim: rule<()>) -> Vec<T>
            = x:r() _ xs:(delim()? _ x:r() { x })* delim()?
            { let mut xs = xs; xs.insert(0, x); xs }
            / { Vec::new() }

        rule integer() -> i32
            = n:$(['0'..='9']+) {? n.parse().or(Err("invalid i32 liternal")) }

        rule string() -> String
            = "\"" n:$([^'"']*) "\"" { n.into() }

        rule abstraction() -> Abstraction
            = "func"
            _ "(" _ vs:optional_delimited(<variable()>, <",">) _ ")"
            _ e:boxed_expr()
            { Abstraction { variables: vs, expr: e } }

        rule variable() -> String
            = v:$(['a'..='z' | 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*)
            { v.into() }

        rule scoped() -> Scoped
            = ds:decls()
            _ "(" _ es:optional_delimited(<expr()>, <";">) _ ";" _ ")"
            { Scoped { decls: ds, exprs: es, value_expr: None } }
            / ds:decls()
            _ "(" _ e:boxed_expr()? _ ")"
            { Scoped { decls: ds, exprs: Vec::new(), value_expr: e} }
            / ds:decls()
            _ "(" _ es:optional_delimited(<expr()>, <";">) _ ")"
            {
                Scoped {
                    decls: ds,
                    exprs: es[..es.len() - 1].to_vec(),
                    value_expr: Some(Box::new(es.last().cloned().unwrap()))
                }
            }

        rule record() -> Vec<(String, ExprO)>
            = "{" _ fs:optional_delimited(<v:variable() _ ":" _ e:expr() { (v, e) }>, <",">) _ "}"
            { fs }

        rule decls() -> Vec<(String, ExprO)>
            = "with"
            _ ds:optional_delimited(<v:variable() _ "=" _ e:expr() { (v, e) }>, <",">)
            { ds }
            / { Vec::new() }

        rule match() -> Match
            = "match" _ e:boxed_expr() _ cs:(case() ** _)
            { Match { variant: e, cases: cs } }

        rule case() -> (String, Option<String>, ExprO)
            // TODO multi-binding
            = "|" _ "{" t:variable() _ v:(":" _ v:variable() { v })? _ "}" _ e:expr()
            { (t, v, e) }
    }
}
