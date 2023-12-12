use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering::SeqCst},
        Arc,
    },
};

use crossbeam_channel::{select, Receiver, Sender};

use crate::{value::ValueType, Evaluator};

type ControlId = u64;

#[derive(Debug)]
pub struct Scheduler {
    ready_tx: Sender<Evaluator>,
    suspend_rx: Receiver<(ControlId, Evaluator)>,
    suspend_tasks: HashMap<ControlId, Vec<Evaluator>>,
    resume_rx: Receiver<ControlId>,
    stop_rx: Receiver<()>,
}

#[derive(Debug)]
pub struct Worker {
    ready_rx: Receiver<Evaluator>,
    ready_tx: Sender<Evaluator>, // for spawning
    suspend_tx: Sender<(ControlId, Evaluator)>,
    resume_tx: Sender<ControlId>,
    control_id: Arc<AtomicU64>,
    task_count: Arc<AtomicUsize>,
    stop_tx: Sender<()>,
    suspend_control: Option<ControlId>,
}

#[derive(Debug)]
pub struct Control {
    id: ControlId,
    resumed: AtomicBool,
    resume_tx: Sender<ControlId>,
}

impl ValueType for Control {
    fn trace(&self) {}
}

impl Scheduler {
    pub fn run(&mut self) -> anyhow::Result<()> {
        loop {
            enum Select {
                Stop(()),
                Suspend((ControlId, Evaluator)),
                Resume(ControlId),
            }
            match select! {
                recv(self.stop_rx) -> msg => Select::Stop(msg?),
                recv(self.suspend_rx) -> msg => Select::Suspend(msg?),
                recv(self.resume_rx) -> msg => Select::Resume(msg?),
            } {
                Select::Stop(()) => break Ok(()),
                Select::Suspend((control_id, task)) => {
                    // TODO assert control id not resumed yet
                    self.suspend_tasks.entry(control_id).or_default().push(task)
                }
                Select::Resume(control_id) => {
                    for task in self.suspend_tasks.remove(&control_id).unwrap_or_default() {
                        self.ready_tx
                            .send(task)
                            .map_err(|_| anyhow::anyhow!("disconnected"))?
                    }
                }
            }
        }
    }
}

impl Worker {
    pub fn run(&mut self) -> anyhow::Result<()> {
        while let Ok(task) = self.ready_rx.recv() {
            //
            if let Some(control_id) = self.suspend_control.take() {
                self.suspend_tx
                    .send((control_id, task))
                    .map_err(|_| anyhow::anyhow!("disconnected"))?
            } else if self.task_count.fetch_sub(1, SeqCst) == 1 {
                self.stop_tx
                    .send(())
                    .map_err(|_| anyhow::anyhow!("disconnected"))?
            }
        }
        Ok(())
    }

    pub fn spawn(&self, task: Evaluator) -> anyhow::Result<()> {
        self.task_count.fetch_add(1, SeqCst);
        self.ready_tx
            .send(task)
            .map_err(|_| anyhow::anyhow!("disconnected"))
    }

    pub fn new_control(&self) -> Control {
        Control {
            id: self.control_id.fetch_add(1, SeqCst),
            resumed: AtomicBool::new(false),
            resume_tx: self.resume_tx.clone(),
        }
    }

    pub fn suspend(&mut self, control: &Control) -> bool {
        if control.resumed.load(SeqCst) {
            false
        } else {
            assert!(self.suspend_control.is_none());
            self.suspend_control = Some(control.id);
            true
        }
    }
}

impl Control {
    pub fn resume(&self) -> anyhow::Result<()> {
        if !self.resumed.swap(true, SeqCst) {
            self.resume_tx
                .send(self.id)
                .map_err(|_| anyhow::anyhow!("disconnected"))?
        }
        Ok(())
    }
}
