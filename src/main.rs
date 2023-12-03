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
    compiler.compile_chunk(expr)?;
    for chunk in &compiler.chunks {
        for (i, instr) in chunk.instrs.iter().enumerate() {
            println!("{i:>4} {instr:?}")
        }
    }
    Ok(())
}
