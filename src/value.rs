use std::{
    any::{type_name, Any},
    collections::HashMap,
    ops::Deref,
};

use crate::{
    compile::Symbol,
    eval::{EvalErrorKind, EvaluatorConsts, I},
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

    fn downcast_ref(value: &Value) -> Option<&Self>
    where
        Self: Sized,
    {
        let Value::Dyn(addr) = value else {
            return None;
        };
        addr.access_shared().any_ref().downcast_ref()
    }

    fn downcast_mut(value: &mut Value) -> Option<&mut Self>
    where
        Self: Sized,
    {
        if let Value::Dyn(addr) = value {
            addr.access_exclusive().any_mut().downcast_mut()
        } else {
            None
        }
    }
}

pub trait ValueTypeExt {
    fn type_name(&self) -> &str;

    fn any_ref(&self) -> &dyn Any;

    fn any_mut(&mut self) -> &mut dyn Any;
}

impl<T: ValueType> ValueTypeExt for T {
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
    // dynamical in both (Rust) type and lifetime
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

impl ValueType for bool {
    fn trace(&self) {}

    fn downcast_ref(value: &Value) -> Option<&Self>
    where
        Self: Sized,
    {
        if let Value::Bool(value) = value {
            Some(value)
        } else {
            None
        }
    }

    fn downcast_mut(value: &mut Value) -> Option<&mut Self>
    where
        Self: Sized,
    {
        if let Value::Bool(value) = value {
            Some(value)
        } else {
            None
        }
    }
}

impl ValueType for u64 {
    fn trace(&self) {}

    fn downcast_ref(value: &Value) -> Option<&Self>
    where
        Self: Sized,
    {
        if let Value::U64(value) = value {
            Some(value)
        } else {
            None
        }
    }

    fn downcast_mut(value: &mut Value) -> Option<&mut Self>
    where
        Self: Sized,
    {
        if let Value::U64(value) = value {
            Some(value)
        } else {
            None
        }
    }
}

impl ValueType for f64 {
    fn trace(&self) {}

    fn downcast_ref(value: &Value) -> Option<&Self>
    where
        Self: Sized,
    {
        if let Value::F64(value) = value {
            Some(value)
        } else {
            None
        }
    }

    fn downcast_mut(value: &mut Value) -> Option<&mut Self>
    where
        Self: Sized,
    {
        if let Value::F64(value) = value {
            Some(value)
        } else {
            None
        }
    }
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

    pub fn downcast_ref<T: ValueType>(&self) -> Result<&T, EvalErrorKind> {
        T::downcast_ref(self).ok_or(EvalErrorKind::TypeError(
            self.type_name().into(),
            type_name::<T>().into(),
        ))
    }

    pub fn downcast_mut<T: ValueType>(&mut self) -> Result<&mut T, EvalErrorKind> {
        // workaround for lifetime error, not sure why it happens
        let name = self.type_name().into();
        T::downcast_mut(self).ok_or(EvalErrorKind::TypeError(name, type_name::<T>().into()))
    }

    fn intrinsic_u64(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        let value = match &r[1] {
            Value::U64(value) => *value,
            Value::I32(value) => *value as _,
            Value::F64(value) => *value as _,
            _ => Err(EvalErrorKind::TypeError(
                r[1].type_name().into(),
                "(Numeric)".into(),
            ))?,
        };
        r[0] = Value::U64(value);
        Ok(())
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

#[derive(Debug, Clone, Default)]
pub struct Vec(std::vec::Vec<Value>);

impl ValueType for Vec {
    fn trace(&self) {}
}

impl Vec {
    fn intrinsic_new(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        r[0] = Value::Dyn(evaluator.allocator.alloc(Vec::default()));
        Ok(())
    }

    fn intrinsic_len(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        let r1 = r[1].downcast_ref::<Vec>()?;
        r[0] = Value::U64(r1.0.len() as _);
        Ok(())
    }

    fn intrinsic_push(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        let r2 = r[2].clone();
        let r1 = r[1].downcast_mut::<Vec>()?;
        r1.0.push(r2);
        r[0] = Value::Unit;
        Ok(())
    }

    fn intrinsic_insert(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        let r2 = *r[2].downcast_ref::<u64>()?;
        let r3 = r[3].clone();
        let r1 = r[1].downcast_mut::<Vec>()?;
        r1.0.insert(r2 as _, r3);
        r[0] = Value::Unit;
        Ok(())
    }

    fn intrinsic_index(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        let r1 = r[1].downcast_ref::<Vec>()?;
        let r2 = r[2].downcast_ref::<u64>()?;
        r[0] =
            r1.0.get(*r2 as usize)
                .ok_or(EvalErrorKind::Panic(format!(
                    "index out of bound: {} >= {}",
                    r2,
                    r1.0.len()
                )))?
                .clone();
        Ok(())
    }

    fn intrinsic_index_mut(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        let r2 = *r[2].downcast_ref::<u64>()?;
        let r3 = r[3].clone();
        let r1 = r[1].downcast_mut::<Vec>()?;
        let r1_len = r1.0.len();
        *r1.0
            .get_mut(r2 as usize)
            .ok_or(EvalErrorKind::Panic(format!(
                "index out of bound: {} >= {}",
                r2, r1_len,
            )))? = r3;
        r[0] = Value::Unit;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Instant(std::time::Instant);

impl ValueType for Instant {
    fn trace(&self) {}
}

impl Instant {
    fn intrinsic_now(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
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
        let r1 = r[1].downcast_ref::<Instant>()?;
        r[0] = Value::F64(r1.0.elapsed().as_secs_f64());
        Ok(())
    }
}

pub fn link(evaluator: &mut EvaluatorConsts) {
    evaluator.link("value_u64", Value::intrinsic_u64);
    evaluator.link("instant_now", Instant::intrinsic_now);
    evaluator.link("instant_elapsed", Instant::intrinsic_elapsed);
    evaluator.link("vec_new", Vec::intrinsic_new);
    evaluator.link("vec_len", Vec::intrinsic_len);
    evaluator.link("vec_push", Vec::intrinsic_push);
    evaluator.link("vec_insert", Vec::intrinsic_insert);
    evaluator.link("vec_index", Vec::intrinsic_index);
    evaluator.link("vec_index_mut", Vec::intrinsic_index_mut);
}
