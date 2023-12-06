use std::{fs::File, io::Write, path::Path};

use compile::CompileError;

use crate::compile::Compiler;

pub mod ast;
pub mod compile;

fn main() -> anyhow::Result<()> {
    let path = std::env::args()
        .nth(1)
        .ok_or(anyhow::anyhow!("no file name provided"))?;
    let path = <_ as AsRef<Path>>::as_ref(&path);
    let source = std::fs::read_to_string(path)?;
    let expr = ast::parse::expr(&source)?;
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
                s.lines().last().map(|l| l.len()).unwrap_or_default()
            )
        }
    }
    result?;
    let mut instr_out = File::create(path.with_extension("instr.txt"))?;
    for chunk_index in 0..compiler.chunks.len() {
        writeln!(instr_out, "{}", compiler.disassemble(chunk_index))?
    }
    Ok(())
}
