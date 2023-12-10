use std::{
    any::{type_name, Any},
    collections::HashMap,
    ops::Deref,
};

use crate::{
    compile::Symbol,
    eval::{EvalErrorKind, I},
    gc::Addr,
    Evaluator,
};

pub trait ValueType
where
    Self: Any + Send + Sync,
{
    fn trace(&self); // TODO

    fn type_name(&self) -> &str {
        type_name::<Self>()
    }
}

pub trait ValueTypeExt {
    fn trace(&self);

    fn type_name(&self) -> &str;

    fn any_ref(&self) -> &dyn Any;

    fn any_mut(&mut self) -> &mut dyn Any;
}

impl<T: ValueType> ValueTypeExt for T {
    fn trace(&self) {
        ValueType::trace(self)
    }

    fn type_name(&self) -> &str {
        ValueType::type_name(self)
    }

    fn any_ref(&self) -> &dyn Any {
        self
    }

    fn any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Invalid,
    // universal representation
    Dyn(Addr),
    // extension to support representing closures with records
    ChunkIndex(usize),
    // extension for common efficiency
    Unit,
    Bool(bool),
    I32(i32),
    U64(u64),
    F64(f64),
}

impl Value {
    pub fn type_name(&self) -> &str {
        match self {
            Self::Invalid => "(Invalid)",
            Self::Dyn(addr) => addr.access_shared().type_name(),
            Self::ChunkIndex(_) => "(Chunk)",
            Self::Unit => "Unit",
            Self::Bool(_) => "Bool",
            Self::I32(_) => "I32",
            Self::U64(_) => "U64",
            Self::F64(_) => "F64",
        }
    }

    pub fn downcast_ref<T: 'static>(&self) -> Option<&T> {
        let Self::Dyn(addr) = self else { return None };
        addr.access_shared().any_ref().downcast_ref()
    }

    pub fn downcast_mut<T: 'static>(&mut self) -> Option<&mut T> {
        let Self::Dyn(addr) = self else { return None };
        addr.access_exclusive().any_mut().downcast_mut()
    }

    pub fn is<T: 'static>(&self) -> bool {
        let Self::Dyn(addr) = self else { return false };
        addr.access_shared().any_ref().is::<T>()
    }
}

pub type Record = HashMap<Symbol, Value>;

impl ValueType for Record {
    fn type_name(&self) -> &str {
        // TODO closure
        "Record"
    }

    fn trace(&self) {}
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct String(pub std::string::String);

impl From<String> for std::string::String {
    fn from(value: String) -> Self {
        value.0
    }
}

impl Deref for String {
    type Target = std::string::String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ValueType for String {
    fn trace(&self) {}
}

pub struct Instant(std::time::Instant);

impl ValueType for Instant {
    fn trace(&self) {}
}

impl Instant {
    fn intrinsic_new(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        r[0] = Value::Dyn(
            evaluator
                .allocator
                .alloc(Instant(std::time::Instant::now())),
        );
        Ok(())
    }

    fn intrinsic_elapsed(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        let r1 = r[1]
            .downcast_ref::<Instant>()
            .ok_or(EvalErrorKind::TypeError(
                r[1].type_name().into(),
                type_name::<Instant>().into(),
            ))?;
        r[0] = Value::F64(r1.0.elapsed().as_secs_f64());
        Ok(())
    }
}

pub fn link(evaluator: &mut Evaluator) {
    evaluator.link("instant_new", Instant::intrinsic_new);
    evaluator.link("instant_elapsed", Instant::intrinsic_elapsed);
}
