use std::ffi::c_void;

use crate::{
    common::{Name, Result, Symbol, sym},
    core::Arity,
    interpreter::{
        Runtime, ensure_type,
        heap::{Bytes, Closure, Code, Table, Value, ValueRepr},
    },
};

pub type PrimFn = fn(&mut Runtime) -> Result<()>;

impl Runtime {
    pub(super) fn register_primitives(&mut self) -> Result<()> {
        self.set_global(&sym("global"), &self.global_env.clone());

        let print_representation = self.make_primitive(prim_print_representation, Arity::Fixed(1));
        self.set_global(&sym("_prim_print_representation"), &print_representation);

        let project = self.make_primitive(prim_project, Arity::VarArg(2, 1));
        self.set_global(&sym("_prim_project"), &project);

        let array_make = self.make_primitive(prim_array_make, Arity::VarArg(1, 0));
        self.set_global(&sym("_prim_array_make"), &array_make);

        let array_match = self.make_primitive(prim_array_match, Arity::Fixed(1));
        self.set_global(&sym("_prim_array_match"), &array_match);

        let bitand = self.make_primitive(prim_bitand, Arity::Fixed(2));
        self.set_global(&sym("_prim_bitand"), &bitand);

        let bitor = self.make_primitive(prim_bitor, Arity::Fixed(2));
        self.set_global(&sym("_prim_bitor"), &bitor);

        let bitxor = self.make_primitive(prim_bitxor, Arity::Fixed(2));
        self.set_global(&sym("_prim_bitxor"), &bitxor);

        let bitnot = self.make_primitive(prim_bitnot, Arity::Fixed(1));
        self.set_global(&sym("_prim_bitnot"), &bitnot);

        let bitshift = self.make_primitive(prim_bitshift, Arity::Fixed(2));
        self.set_global(&sym("_prim_bitshift"), &bitshift);

        let custom_match = self.make_primitive(prim_match, Arity::Fixed(1));
        self.set_global(&sym("_prim_match"), &custom_match);

        let string_print = self.make_primitive(prim_string_print, Arity::Fixed(1));
        self.set_global(&sym("_prim_string_print"), &string_print);

        let to_string = self.make_primitive(prim_to_string, Arity::Fixed(1));
        self.set_global(&sym("_prim_to_string"), &to_string);

        let load = self.make_primitive(prim_load, Arity::Fixed(1));
        self.set_global(&sym("_prim_load"), &load);

        let symbol_make = self.make_primitive(prim_symbol_make, Arity::Fixed(1));
        self.set_global(&sym("_prim_symbol_make"), &symbol_make);

        let ceiling = self.make_primitive(prim_ceiling, Arity::Fixed(1));
        self.set_global(&sym("_prim_ceiling"), &ceiling);




        Ok(())
    }

    pub(super) fn register_type_tables(&mut self) -> Result<()> {
        // Since strings created without the string table present will
        // not have the string type, create that first, and
        // fix up the typeid type.
        self.register_type("string", "string");
        let string_table_id = self.get_name(&Name::str("string.typeid")).unwrap();
        let byte_str = string_table_id.get_bytes();
        let string_table = self.get_global(&sym("string"));
        byte_str.set_type_table(string_table);
        self.register_type("nil_type", "nil");
        self.register_type("bool", "bool");
        self.register_type("int", "int");
        self.register_type("float", "float");
        self.register_type("char", "char");
        self.register_type("pointer", "pointer");
        self.register_type("symbol", "symbol");
        self.register_type("bytes", "bytes");
        self.register_type("array", "array");
        self.register_type("table", "table");
        self.register_type("function", "function");
        Ok(())
    }

    fn register_type(&mut self, table_name: &str, type_name: &str) {
        let new_table = Value::from(Table::make(self));
        self.set_global(&sym(table_name), &new_table);
        let tname = self.make_string(type_name);
        self.add_name(&Name::str(&format!("{}.type_id", table_name)), &tname)
            .unwrap();
    }

    pub(super) fn register_system_constants(&mut self) -> Result<()> {
        let os = self.make_string(std::env::consts::OS);
        self.add_name(&Name::str("system.os"), &os)?;
        let family = self.make_string(std::env::consts::FAMILY);
        self.add_name(&Name::str("system.os_family"), &family)?;
        let arch = self.make_string(std::env::consts::ARCH);
        self.add_name(&Name::str("system.arch"), &arch)?;
        let dll_ext = self.make_string(std::env::consts::DLL_EXTENSION);
        self.add_name(&Name::str("system.dll_extension"), &dll_ext)?;
        let dll_prefix = self.make_string(std::env::consts::DLL_PREFIX);
        self.add_name(&Name::str("system.dll_prefix"), &dll_prefix)?;
        let exe_ext = self.make_string(std::env::consts::EXE_EXTENSION);
        self.add_name(&Name::str("system.exe_extension"), &exe_ext)?;
        Ok(())
    }

    pub fn make_primitive(&mut self, fun: fn(&mut Runtime) -> Result<()>, arity: Arity) -> Value {
        let code = Code::Extern(fun as *const c_void);
        let (args, vararg) = match arity {
            Arity::Fixed(n) => (n as usize, None),
            Arity::VarArg(n, v) => (n as usize, Some(v as usize)),
        };
        let closure = Closure::make(self, &code, &[], args, vararg);
        Value::from(closure)
    }

    pub fn make_string(&mut self, s: &str) -> Value {
        let b = Bytes::with(self, s.as_bytes());
        let type_table = self
            .get_module_path(&[Symbol::from("string")], self.global_env).unwrap_or(Value::nil());
        // If the string type table doesn't exist yet, leave it
        // as nil. This will happen when the string type table
        // is registered. It can patch it up afterwards.
        b.set_type_table(type_table);
        Value::from(b)
    }
}

fn prim_print_representation(rt: &mut Runtime) -> Result<()> {
    let val = rt.stack.pop();
    print!("{:?}", val);
    Ok(())
}

fn prim_project(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_array_make(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_bitand(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_bitor(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_bitxor(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_bitnot(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_bitshift(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_array_match(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_match(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_string_print(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_to_string(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_load(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_symbol_name(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_symbol_make(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_bytes_make(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_table_keys(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_table_delete(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_exit(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_panic(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_string_pack(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_string_unpack(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_string_substring(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_to_char(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_ceiling(rt: &mut Runtime) -> Result<()> {
    let x = rt.stack.pop();
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().ceil();
    let alloced = Value::alloc_float(rt, res);
    rt.stack_push(alloced);
    Ok(())
}

fn prim_floor(rt: &mut Runtime) -> Result<()> {
    let x = rt.stack.pop();
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().floor();
    let alloced = Value::alloc_float(rt, res);
    rt.stack_push(alloced);
    Ok(())
}

fn prim_round(rt: &mut Runtime) -> Result<()> {
    let x = rt.stack.pop();
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().round();
    let alloced = Value::alloc_float(rt, res);
    rt.stack_push(alloced);
    Ok(())
}

fn prim_sin(rt: &mut Runtime) -> Result<()> {
    let x = rt.stack.pop();
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().sin();
    let alloced = Value::alloc_float(rt, res);
    rt.stack_push(alloced);
    Ok(())
}

fn prim_cos(rt: &mut Runtime) -> Result<()> {
    let x = rt.stack.pop();
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().cos();
    let alloced = Value::alloc_float(rt, res);
    rt.stack_push(alloced);
    Ok(())
}

fn prim_tan(rt: &mut Runtime) -> Result<()> {
    let x = rt.stack.pop();
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().tan();
    let alloced = Value::alloc_float(rt, res);
    rt.stack_push(alloced);
    Ok(())
}

fn prim_asin(rt: &mut Runtime) -> Result<()> {
    let x = rt.stack.pop();
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().asin();
    let alloced = Value::alloc_float(rt, res);
    rt.stack_push(alloced);
    Ok(())
}

fn prim_acos(rt: &mut Runtime) -> Result<()> {
    let x = rt.stack.pop();
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().acos();
    let alloced = Value::alloc_float(rt, res);
    rt.stack_push(alloced);
    Ok(())
}

fn prim_atan(rt: &mut Runtime) -> Result<()> {
    let x = rt.stack.pop();
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().atan();
    let alloced = Value::alloc_float(rt, res);
    rt.stack_push(alloced);
    Ok(())
}

fn prim_log(rt: &mut Runtime) -> Result<()> {
    let x = rt.stack.pop();
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().ln();
    let alloced = Value::alloc_float(rt, res);
    rt.stack_push(alloced);
    Ok(())
}

fn prim_exp(rt: &mut Runtime) -> Result<()> {
    let x = rt.stack.pop();
    ensure_type(&x, ValueRepr::Float)?;
    let res = x.get_float().exp();
    let alloced = Value::alloc_float(rt, res);
    rt.stack_push(alloced);
    Ok(())
}

fn prim_to_int(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_to_pointer(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_foreign_call(rt: &mut Runtime) -> Result<()> {
    Ok(())
}