use crate::compile::Compiler;

pub mod ast;
pub mod compile;

fn main() -> anyhow::Result<()> {
    let source = std::fs::read_to_string(
        std::env::args()
            .nth(1)
            .ok_or(anyhow::anyhow!("no file name provided"))?,
    )?;
    let expr = ast::parse::expr(&source)?;
    println!("{:?}", expr);
    let mut compiler = Compiler::default();
    let chunk_index = compiler.compile_chunk(expr)?;
    println!("{}", compiler.disassemble(chunk_index));
    Ok(())
}
