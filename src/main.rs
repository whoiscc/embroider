pub mod ast;
pub mod compile;
pub mod eval;
pub mod gc;
pub mod sched;
pub mod value;

use crossbeam_channel::{unbounded, Receiver, Sender};

pub use crate::compile::Compiler;
pub use crate::eval::Evaluator;
pub use crate::value::Value;

use std::thread::{spawn, JoinHandle};
use std::{fs::File, io::Write, path::Path};

use crate::compile::CompileError;
use crate::eval::EvaluatorConsts;
use crate::sched::new_system;

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
    let (mut scheduler, workers) = new_system(EvaluatorConsts::new(compiler), chunk_index);

    let n = std::thread::available_parallelism()?.get();
    let group = StopGroup::new(n);
    let scheduler = group.spawn(move |stop_rx| scheduler.run(stop_rx));
    let workers = workers
        .map(|mut worker| group.spawn(move |stop_rx| worker.run(stop_rx)))
        .take(std::thread::available_parallelism()?.get())
        .collect::<Vec<_>>();
    scheduler.join().unwrap()?;
    for worker in workers {
        worker.join().unwrap()?
    }
    Ok(())
}

#[derive(Debug, Clone)]
struct StopGroup {
    tx: Sender<()>,
    rx: Receiver<()>,
    n: usize,
}

impl StopGroup {
    fn new(n: usize) -> Self {
        let (tx, rx) = unbounded();
        Self { tx, rx, n }
    }

    fn spawn(
        &self,
        task: impl FnOnce(Receiver<()>) -> anyhow::Result<()> + Send + 'static,
    ) -> JoinHandle<anyhow::Result<()>> {
        let this = self.clone();
        spawn(move || {
            let result = task(this.rx);
            for _ in 0..this.n {
                this.tx.send(()).unwrap()
            }
            result
        })
    }
}
