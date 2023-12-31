pub type ExprO = (Expr, usize);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Integer(i32),
    String(String),
    Record(Vec<(String, ExprO)>),
    Abstraction(Abstraction),

    Import(String),

    Variable(String),

    GetField(Box<ExprO>, String),
    Scoped(Scoped),
    Operator(Operator, Vec<ExprO>),
    Apply(Box<ExprO>, Vec<ExprO>),

    Mut(String, Box<ExprO>),
    MutField(Box<ExprO>, String, Box<ExprO>),
    Capture(String, String), // currently unused

    Return(Box<ExprO>),
    Break,
    Continue,

    Match(Match),
    Loop(Box<ExprO>),

    // the concurrent primitives can be expressed as applying and do not require dedicated syntax
    // these syntax are for evaluator to identify them from bytecode, this is necessary because 
    // (currently) intrinsics have no access to `Worker` and these primitives cannot be implemented
    // as intrinsics
    Spawn(Box<ExprO>),
    Control,
    Suspend(Box<ExprO>),
    Resume(Box<ExprO>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Abstraction {
    pub variables: Vec<String>,
    pub captures: Vec<String>,
    pub lang: Option<String>,
    pub expr: Box<ExprO>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scoped {
    pub decls: Vec<(String, ExprO)>,
    pub exprs: Vec<ExprO>,
    pub value_expr: Option<Box<ExprO>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Match {
    pub variant: Box<ExprO>,
    pub cases: Vec<(String, Option<String>, ExprO)>,
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

    Lsh,
    Rsh,
    Xor,
    Band,
    Bor,
}

peg::parser! {
    pub grammar parse() for str {
        // for trailing comment
        pub rule program() -> ExprO = e:expr() _ { e }

        rule boxed_expr() -> Box<ExprO> = e:expr() { Box::new(e) }
        rule expr() -> ExprO = precedence! {
            p:position!() e:@ _:position!() { (e, p) }
            --
            e:@ _ "mut." v:variable() _ m:boxed_expr() { Expr::MutField(Box::new(e), v, m) }
            v:variable() _ "mut" _ m:boxed_expr() { Expr::Mut(v, m) }
            // v:variable() _ "capture" _ c:variable() { Expr::Capture(v, c) }
            --
            e:(@) _ "or" _ x:@ { Expr::Operator(Operator::Or, vec![e, x]) }
            --
            e:(@) _ "and" _ x:@ { Expr::Operator(Operator::And, vec![e, x]) }
            --
            e:(@) _ "==" _ x:@ { Expr::Operator(Operator::Eq, vec![e, x]) }
            e:(@) _ "!=" _ x:@ { Expr::Operator(Operator::Ne, vec![e, x]) }
            e:(@) _ "<=" _ x:@ { Expr::Operator(Operator::Le, vec![e, x]) }
            e:(@) _ ">=" _ x:@ { Expr::Operator(Operator::Ge, vec![e, x]) }
            e:(@) _ "<" _ x:@ { Expr::Operator(Operator::Lt, vec![e, x]) }
            e:(@) _ ">" _ x:@ { Expr::Operator(Operator::Gt, vec![e, x]) }
            --
            e:(@) _ "|" _ x:@ { Expr::Operator(Operator::Bor, vec![e, x]) }
            --
            e:(@) _ "^" _ x:@ { Expr::Operator(Operator::Xor, vec![e, x]) }
            --
            e:(@) _ "&" _ x:@ { Expr::Operator(Operator::Band, vec![e, x]) }
            --
            e:(@) _ "<<" _ x:@ { Expr::Operator(Operator::Lsh, vec![e, x]) }
            e:(@) _ ">>" _ x:@ { Expr::Operator(Operator::Rsh, vec![e, x]) }
            --
            e:(@) _ "+" _ x:@ { Expr::Operator(Operator::Add, vec![e, x]) }
            e:(@) _ "-" _ x:@ { Expr::Operator(Operator::Sub, vec![e, x]) }
            --
            e:(@) _ "*" _ x:@ { Expr::Operator(Operator::Mul, vec![e, x]) }
            e:(@) _ "/" _ x:@ { Expr::Operator(Operator::Div, vec![e, x]) }
            e:(@) _ "%" _ x:@ { Expr::Operator(Operator::Rem, vec![e, x]) }
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
            "spawn" _ e:boxed_expr() { Expr::Spawn(e) }
            "control" { Expr::Control }
            "suspend" _ e:boxed_expr() { Expr::Suspend(e) }
            "resume" _ e:boxed_expr() { Expr::Resume(e) }
            "import" _ s:string() { Expr::Import(s) }
            e:variable() { Expr::Variable(e) }
        }

        rule _ = quiet!{(" " / "\n" / ("#" [^'\n']*))*}

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
            _ l:("lang" _ l:variable() { l })?
            _ cs:("capture" _ v:variable() _ { v })*
            _ e:boxed_expr()
            { Abstraction { variables: vs, captures: cs, lang: l, expr: e } }

        rule variable() -> String
            = v:$(['a'..='z' | 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*)
            { v.into() }

        rule scoped() -> Scoped
            // a relatively compact representation, outer layer list for list of `with`, and inner
            // layer list for "concurrent" declarations from each `with`. so
            //     with a = ...
            //     with b = ..., c = ... (expr)
            // will be [[(a, ...)], [(b, ...), (c, ...)]]
            // there was a time where `with` is prepended to decalarations whenever possible, and
            // this compacted representation can avoid terribly deep nested AST
            // this does not cause problem after i revert the coding style, so just save it in case
            // needed
            = ds:decls() _ s:scoped() { let mut s = s; s.decls.splice(0..0, ds); s }
            / ds:decls() _ es:scoped_exprs()
            { Scoped { decls: ds, exprs: es.0, value_expr: es.1 } }
            / es:scoped_exprs()
            { Scoped { decls: Vec::new(), exprs: es.0, value_expr: es.1 } }

        rule scoped_exprs() -> (Vec<ExprO>, Option<Box<ExprO>>)
            = "(" _ es:optional_delimited(<expr()>, <";">) _ ";" _ ")"
            { (es, None) }
            / "(" _ e:boxed_expr()? _ ")"
            { (Vec::new(), e) }
            / "(" _ es:optional_delimited(<expr()>, <";">) _ ")"
            { (es[..es.len() - 1].to_vec(), Some(Box::new(es.last().cloned().unwrap()))) }

        rule record() -> Vec<(String, ExprO)>
            = "{" _ fs:optional_delimited(<v:variable() _ ":" _ e:expr() { (v, e) }>, <",">) _ "}"
            { fs }

        rule decls() -> Vec<(String, ExprO)>
            = "with"
            _ ds:optional_delimited(<v:variable() _ "=" _ e:expr() { (v, e) }>, <",">)
            { ds }

        rule match() -> Match
            = "match" _ e:boxed_expr() _ cs:(case() ** _)
            { Match { variant: e, cases: cs } }

        rule case() -> (String, Option<String>, ExprO)
            // TODO multi-binding
            = "|" _ "{" t:variable() _ v:(":" _ v:variable() { v })? _ "}" _ e:expr()
            { (t, v, e) }
    }
}
