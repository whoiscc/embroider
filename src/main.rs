use std::{fs::File, io::Write, path::Path};

pub mod ast;
pub mod compile;
pub mod eval;
pub mod gc;
pub mod value;

use gc::Allocator;

pub use crate::compile::Compiler;
pub use crate::eval::Evaluator;
pub use crate::value::Value;

use crate::compile::CompileError;

fn main() -> anyhow::Result<()> {
    let path = std::env::args()
        .nth(1)
        .ok_or(anyhow::anyhow!("no file name provided"))?;
    let path = <_ as AsRef<Path>>::as_ref(&path);
    let source = std::fs::read_to_string(path)?;
    let expr = ast::parse::program(&source)?;
    let mut ast_out = File::create(path.with_extension("ast.txt"))?;
    writeln!(ast_out, "{expr:#?}")?;
    let mut compiler = Compiler::default();
    let result = compiler.compile_chunk(expr);
    if let Err(err) = &result {
        if let Some(CompileError(_, offset)) = err.downcast_ref() {
            let s = &source[..*offset];
            println!(
                "line {} col {}",
                s.lines().count(),
                s.lines().last().unwrap_or_default().len() + 1,
            )
        }
    }
    let chunk_index = result?;
    let mut instr_out = File::create(path.with_extension("instr.txt"))?;
    for chunk_index in 0..compiler.chunks.len() {
        writeln!(instr_out, "{}", compiler.disassemble(chunk_index))?
    }
    let mut evaluator = Evaluator::new(compiler, Allocator::default());
    evaluator.eval_chunk(chunk_index)
}
