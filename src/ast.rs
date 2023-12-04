#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Integer(u64),
    String(String),
    Record(Vec<(String, Box<Expr>)>),
    Abstraction(Abstraction),

    Variable(String),

    GetField(Box<Expr>, String),
    Scoped(Scoped),
    Operator(Operator, Vec<Expr>),
    Apply(Box<Expr>, Vec<Expr>),

    Mut(String, Box<Expr>),
    MutField(Box<Expr>, String, Box<Expr>),

    Return(Box<Expr>),
    Break,
    Continue,

    Match(Match),
    Loop(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Abstraction {
    pub variables: Vec<String>,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scoped {
    pub decls: Vec<(String, Expr)>,
    pub exprs: Vec<Expr>,
    pub value_expr: Option<Box<Expr>>,
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
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Match {
    pub variant: Box<Expr>,
    pub cases: Vec<(String, String, Expr)>,
}

peg::parser! {
    pub grammar parse() for str {
        pub rule boxed_expr() -> Box<Expr> = e:expr() { Box::new(e) }

        pub rule expr() -> Expr = precedence! {
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
            "not" _ e:@ { Expr::Operator(Operator::Not, vec![e]) }
            --
            e:@ "." v:variable() { Expr::GetField(Box::new(e), v) }
            e:@ "(" _ es:optional_delimited(<expr()>, <",">) _ ")"
                { Expr::Apply(Box::new(e), es) }
            --
            e:integer() { Expr::Integer(e) }
            e:string() { Expr::String(e) }
            e:abstraction() { Expr::Abstraction(e) }
            e:scoped() { Expr::Scoped(e) }
            "{" _ ds:optional_delimited(<decl()>, <",">) _ "}"
                { Expr::Record(ds.into_iter().map(|(n, e)| (n, Box::new(e))).collect()) }
            e:match() { Expr::Match(e) }
            "loop" _ e:boxed_expr() { Expr::Loop(e) }
            "return" _ e:boxed_expr() { Expr::Return(e) }
            "break" { Expr::Break }
            "continue" { Expr::Continue }
            e:variable() { Expr::Variable(e) }
        }

        rule _ = quiet!{[' ' | '\n']*}

        rule optional_delimited<T>(x: rule<T>, delim: rule<()>) -> Vec<T>
            = xs:(x() ** ((_ delim() _)?)) _ delim()? { xs }

        rule integer() -> u64
            = n:$(['0'..='9']+) {? n.parse().or(Err("invalid u64 liternal")) }

        rule string() -> String
            = "\"" n:$([^'"']*) "\"" { n.into() }

        rule abstraction() -> Abstraction
            = "func"
            _ "(" _ vs:optional_delimited(<variable()>, <",">) _ ")"
            _ e:boxed_expr()
            { Abstraction { variables: vs, expr: e } }

        rule variable() -> String
            = "_" { "*".into() }
            / v:$(['a'..='z' | 'A'..='Z'] ['a'..='z' | 'A'..='Z' | '0'..='9']*)
            { v.into() }

        rule scoped() -> Scoped
            = ds:decls()
            _ "(" _ es:optional_delimited(<expr()>, <";">) _ e:boxed_expr() ")"
            { Scoped { decls: ds, exprs: es, value_expr: Some(e) } }
            / ds:decls()
            _ "(" _ es:optional_delimited(<expr()>, <";">) _ ";" _ ")"
            { Scoped { decls: ds, exprs: es, value_expr: None } }
            / ds:decls()
            _ "(" _ e:boxed_expr() _ ")"
            { Scoped { decls: ds, exprs: Vec::new(), value_expr: Some(e)} }

        rule decls() -> Vec<(String, Expr)>
            = "with" _ ds:optional_delimited(<decl()>, <_ "," _>) { ds }
            / { Vec::new() }

        rule decl() -> (String, Expr)
            = v:variable() _ "=" _ e:expr() { (v, e) }

        rule match() -> Match
            = "match" _ e:boxed_expr() _ cs:(case() ** _)
            { Match { variant: e, cases: cs } }

        rule case() -> (String, String, Expr)
            = "|" _ t:variable() _ v:variable() _ e:expr() { (t, v, e) }
    }
}
