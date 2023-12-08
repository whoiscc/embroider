use std::{
    any::{type_name, Any},
    collections::HashMap,
};

use crate::{compile::Symbol, gc::Addr};

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

impl ValueType for String {
    fn trace(&self) {}
}
