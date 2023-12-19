use embroider::{
    eval::{EvalErrorKind, EvaluatorConsts, I},
    value::{self, ValueType},
    Evaluator, Value,
};

fn intrinsic_print(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
    let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
    let r0 = r[0].downcast_ref::<value::String>()?;
    println!("{}", &**r0);
    r[0] = Value::Unit;
    Ok(())
}

fn intrinsic_repr(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
    let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
    let repr = format!("{:?}", r[0]);
    r[0] = Value::Dyn(evaluator.allocator.alloc(value::String(repr)));
    // println!("{:?}", r[0]);
    // println!("{}", r[0].type_name());
    Ok(())
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
        let r0 = r[0].downcast_ref::<Vec>()?;
        r[0] = Value::U64(r0.0.len() as _);
        Ok(())
    }

    fn intrinsic_push(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        let r1 = r[1].clone();
        let r0 = r[0].downcast_mut::<Vec>()?;
        r0.0.push(r1);
        r[0] = Value::Unit;
        Ok(())
    }

    fn intrinsic_insert(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        let r1 = *r[1].downcast_ref::<u64>()?;
        let r2 = r[2].clone();
        let r0 = r[0].downcast_mut::<Vec>()?;
        r0.0.insert(r1 as _, r2);
        r[0] = Value::Unit;
        Ok(())
    }

    fn intrinsic_index(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        let r0 = r[0].downcast_ref::<Vec>()?;
        let r1 = r[1].downcast_ref::<u64>()?;
        r[0] =
            r0.0.get(*r1 as usize)
                .ok_or(EvalErrorKind::Panic(format!(
                    "index out of bound: {} >= {}",
                    r1,
                    r0.0.len()
                )))?
                .clone();
        Ok(())
    }

    fn intrinsic_index_mut(evaluator: &mut Evaluator) -> Result<(), EvalErrorKind> {
        let mut r = I(&mut evaluator.registers, evaluator.intrinsic_base_pointer);
        let r1 = *r[1].downcast_ref::<u64>()?;
        let r2 = r[2].clone();
        let r0 = r[0].downcast_mut::<Vec>()?;
        let r0_len = r0.0.len();
        *r0.0
            .get_mut(r1 as usize)
            .ok_or(EvalErrorKind::Panic(format!(
                "index out of bound: {} >= {}",
                r1, r0_len,
            )))? = r2;
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
        let r0 = r[0].downcast_ref::<Instant>()?;
        r[0] = Value::F64(r0.0.elapsed().as_secs_f64());
        Ok(())
    }
}

pub fn link(evaluator: &mut EvaluatorConsts) {
    evaluator.link("print", intrinsic_print);
    evaluator.link("repr", intrinsic_repr);
    evaluator.link("instant_now", Instant::intrinsic_now);
    evaluator.link("instant_elapsed", Instant::intrinsic_elapsed);
    evaluator.link("vec_new", Vec::intrinsic_new);
    evaluator.link("vec_len", Vec::intrinsic_len);
    evaluator.link("vec_push", Vec::intrinsic_push);
    evaluator.link("vec_insert", Vec::intrinsic_insert);
    evaluator.link("vec_index", Vec::intrinsic_index);
    evaluator.link("vec_index_mut", Vec::intrinsic_index_mut);
}
