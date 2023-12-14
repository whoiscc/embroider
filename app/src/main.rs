use std::{fs::File, io::Write, path::Path};

use embroider::sched::StopGroup;
use embroider::{ast, Compiler};
use embroider::{compile::CompileError, eval::EvaluatorConsts, sched::new_system};

fn main() -> anyhow::Result<()> {
    let mut module = std::env::args()
        .nth(1)
        .ok_or(anyhow::anyhow!("no file name provided"))?;
    let auxiliary_dir = Path::new("./target/lang");
    std::fs::create_dir_all(auxiliary_dir)?;
    let mut compiler = Compiler::default();
    let mut chunk_index = None;
    while let Some(imported_module) = {
        let path = Path::new("./lang").join(&module).with_extension("txt");
        let source = std::fs::read_to_string(path)?;
        let expr = ast::parse::program(&source)?;
        let mut ast_out = File::create(auxiliary_dir.join(&module).with_extension("ast.txt"))?;
        writeln!(ast_out, "{expr:#?}")?;
        let result = compiler.compile_module(module.clone(), expr);
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
        chunk_index.get_or_insert(result?);
        let mut instr_out = File::create(auxiliary_dir.join(&module).with_extension("instr.txt"))?;
        for chunk_index in 0..compiler.chunks.len() {
            writeln!(instr_out, "{}", compiler.disassemble(chunk_index))?
        }
        compiler.next_imported()
    } {
        module = imported_module.to_string()
    }
    let mut eval_consts = EvaluatorConsts::new(compiler);
    embroider_std::link(&mut eval_consts);
    let workers = new_system(eval_consts, chunk_index.unwrap());

    let group = StopGroup::default();
    let workers = workers
        .map(|mut worker| group.spawn(move |stop_rx| worker.run(stop_rx)))
        .take(std::thread::available_parallelism()?.get())
        .collect::<Vec<_>>();
    drop(group);
    for worker in workers {
        worker.join().unwrap()?
    }
    Ok(())
}
