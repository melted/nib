use std::ffi::c_void;

use crate::{common::{Result, Symbol}, core::Arity, interpreter::{ensure_type, heap::{Closure, Code, Value, ValueRepr}, Runtime}};

pub type PrimFn = fn (&mut Runtime)->Result<()>;

impl Runtime {
    pub(super) fn register_primitives(&mut self) -> Result<()> {
        let prim_ceiling = self.make_closure(prim_ceiling, Arity::Fixed(1));
        self.set_global(&Symbol::from("_prim_ceiling"), &prim_ceiling);
        Ok(())         
    }

    fn make_closure(&mut self, fun : fn (&mut Runtime)->Result<()>, arity: Arity) -> Value {
        let code = Code::Extern(fun as *const c_void);
        let (args, vararg) = match arity {
            Arity::Fixed(n) => (n as usize, None),
            Arity::VarArg(n, v) => (n as usize, Some(v as usize))
        };
        let closure = Closure::make(&mut self.heap, &code, &[], args, vararg);
        Value::from(closure)
    }
}

fn prim_ceiling(rt: &mut Runtime) -> Result<()> {
    let x = rt.regs[2];
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().ceil();
    rt.regs[1] = Value::alloc_float(&mut rt.heap, res);
    Ok(())
}
