pub mod ast;

fn main() -> anyhow::Result<()> {
    println!(
        "{:?}",
        ast::parse::expr(&std::fs::read_to_string(
            std::env::args()
                .nth(1)
                .ok_or(anyhow::anyhow!("no file name provided"))?
        )?)
    );
    Ok(())
}
