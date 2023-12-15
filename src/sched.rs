use std::{
    iter::repeat,
    mem::replace,
    sync::{
        atomic::{AtomicUsize, Ordering::SeqCst},
        Arc, Mutex,
    },
};

use crossbeam_channel::{bounded, select, unbounded, Receiver, Sender};

use crate::{
    compile::ChunkIndex, eval::EvaluatorConsts, gc::Allocator, value::ValueType, Evaluator, Value,
};

#[derive(Debug, Clone)]
pub struct Worker {
    ready_rx: Receiver<Evaluator>,
    ready_tx: Sender<Evaluator>,
    task_count: Arc<AtomicUsize>,
    suspend_control: Option<Value>,
    eval_consts: Arc<EvaluatorConsts>,
}

#[derive(Debug)]
pub struct Control {
    state: Mutex<ControlState>,
    ready_tx: Sender<Evaluator>,
}

#[derive(Debug)]
enum ControlState {
    Blocking(Vec<Evaluator>),
    Resumed,
}

impl ValueType for Control {
    fn trace(&self) {}
}

impl Worker {
    pub fn run(&mut self, stop_rx: Receiver<()>) -> anyhow::Result<()> {
        'next_task: loop {
            let mut task = select! {
                recv(stop_rx) -> msg => break Ok(msg?),
                recv(self.ready_rx) -> msg => msg?,
            };
            while let Some(value) = {
                task.eval(self)?;
                self.suspend_control.take()
            } {
                let control = value.downcast_ref::<Control>().unwrap();
                if let ControlState::Blocking(tasks) = &mut *control
                    .state
                    .lock()
                    .map_err(|_| anyhow::anyhow!("lock poisoned"))?
                {
                    // println!("suspending");
                    tasks.push(task);
                    continue 'next_task;
                }
                // println!("skip suspending");
            }
            if self.task_count.fetch_sub(1, SeqCst) == 1 {
                break Ok(());
            }
        }
    }

    pub fn spawn(&self, chunk_value: Value) -> anyhow::Result<()> {
        self.task_count.fetch_add(1, SeqCst);
        let mut task = Evaluator::new(self.eval_consts.clone(), Allocator::default());
        task.push_entry_frame(chunk_value);
        self.ready_tx
            .send(task)
            .map_err(|_| anyhow::anyhow!("disconnected"))
    }

    pub fn new_control(&self) -> Control {
        Control {
            state: Mutex::new(ControlState::Blocking(Default::default())),
            ready_tx: self.ready_tx.clone(),
        }
    }

    pub fn suspend(&mut self, value: Value) {
        assert!(self.suspend_control.is_none());
        self.suspend_control = Some(value)
    }
}

impl Control {
    pub fn resume(&self) -> anyhow::Result<()> {
        if let ControlState::Blocking(tasks) = replace(
            &mut *self
                .state
                .lock()
                .map_err(|_| anyhow::anyhow!("lock poisoned"))?,
            ControlState::Resumed,
        ) {
            // println!("resuming");
            for task in tasks {
                self.ready_tx
                    .send(task)
                    .map_err(|_| anyhow::anyhow!("disconnected"))?
            }
        }
        Ok(())
    }
}

pub fn new_system(
    eval_consts: impl Into<Arc<EvaluatorConsts>>,
    chunk_index: ChunkIndex,
) -> impl Iterator<Item = Worker> {
    let (ready_tx, ready_rx) = unbounded();

    let eval_consts = eval_consts.into();
    let mut task = Evaluator::new(eval_consts.clone(), Allocator::default());
    task.push_entry_frame(Value::ChunkIndex(chunk_index));
    ready_tx.send(task).unwrap();

    repeat(Worker {
        ready_rx,
        ready_tx,
        task_count: Arc::new(AtomicUsize::new(1)),
        eval_consts,
        suspend_control: None,
    })
}

#[derive(Debug, Clone)]
pub struct StopGroup {
    tx: Sender<()>,
    rx: Receiver<()>,
}

impl Default for StopGroup {
    fn default() -> Self {
        let (tx, rx) = bounded(0);
        Self { tx, rx }
    }
}

impl StopGroup {
    pub fn spawn(&self, mut worker: Worker) -> std::thread::JoinHandle<anyhow::Result<()>> {
        let this = self.clone();
        std::thread::spawn(move || {
            let result = worker.run(this.rx);
            while this.tx.send(()).is_ok() {}
            result
        })
    }
}
