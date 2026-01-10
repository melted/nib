use std::{collections::{HashMap, HashSet}, ffi::c_void, sync::LazyLock};

use symbol_table::static_symbol;

use crate::{
    common::{Name, Result, Symbol, sym},
    core::Arity,
    interpreter::{
        Runtime, bytecode::{INSTR_ACOS, INSTR_ADD, INSTR_ALLOC_ARRAY, INSTR_ALLOC_BYTES, INSTR_ALLOC_TABLE, INSTR_ARRAY_REF, INSTR_ARRAY_SET, INSTR_ARRAY_SIZE, INSTR_ASIN, INSTR_ATAN, INSTR_BITAND, INSTR_BITNOT, INSTR_BITOR, INSTR_BITSHIFT, INSTR_BITXOR, INSTR_BYTES_REF, INSTR_BYTES_SET, INSTR_BYTES_SIZE, INSTR_CEILING, INSTR_CMP, INSTR_COS, INSTR_DIV, INSTR_EQ, INSTR_EXP, INSTR_FLOOR, INSTR_LOG, INSTR_MOD, INSTR_MUL, INSTR_NEG, INSTR_ROUND, INSTR_SET_TYPE, INSTR_SIN, INSTR_SUB, INSTR_TABLE_DELETE, INSTR_TABLE_GET, INSTR_TABLE_SET, INSTR_TABLE_SIZE, INSTR_TAN, INSTR_TOINT, INSTR_TYPE}, ensure_type, heap::{Bytes, Closure, Code, Table, Value, ValueRepr}
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
        self.register_type("call_continuation", "call_continuation");
        self.register_type("partial_application", "partial_application");
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
            .get_module_path(&[static_symbol!("string")], self.global_env)
            .unwrap_or(Value::nil());
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

fn prim_to_pointer(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

fn prim_foreign_call(rt: &mut Runtime) -> Result<()> {
    Ok(())
}

/// Primitives that are implemented as bytecode instructions
/// rather than calling out to a function.
fn is_bytecode_primitive(prim:&Symbol) -> Option<u8> {
    static BYTECODE_PRIMS:LazyLock<HashMap<Symbol, u8>> = LazyLock::new(|| {
        let mut prims = HashMap::new();
        prims.insert(static_symbol!("__prim_add"), INSTR_ADD);
        prims.insert(static_symbol!("__prim_sub"), INSTR_SUB);
        prims.insert(static_symbol!("__prim_mul"), INSTR_MUL);
        prims.insert(static_symbol!("__prim_div"), INSTR_DIV);
        prims.insert(static_symbol!("__prim_mod"), INSTR_MOD);
        prims.insert(static_symbol!("__prim_bitand"), INSTR_BITAND);
        prims.insert(static_symbol!("__prim_bitor"), INSTR_BITOR);
        prims.insert(static_symbol!("__prim_bitxor"), INSTR_BITXOR);
        prims.insert(static_symbol!("__prim_bitshift"), INSTR_BITSHIFT);
        prims.insert(static_symbol!("__prim_bitnot"), INSTR_BITNOT);
        prims.insert(static_symbol!("__prim_type"), INSTR_TYPE);
        prims.insert(static_symbol!("__prim_type_set"), INSTR_SET_TYPE);
        prims.insert(static_symbol!("__prim_negate"), INSTR_NEG);
        prims.insert(static_symbol!("__prim_gte"), INSTR_CMP);
        prims.insert(static_symbol!("__prim_gt"), INSTR_CMP);
        prims.insert(static_symbol!("__prim_lte"), INSTR_CMP);
        prims.insert(static_symbol!("__prim_lt"), INSTR_CMP); 
        prims.insert(static_symbol!("__prim_eq"), INSTR_EQ);
        prims.insert(static_symbol!("__prim_sin"), INSTR_SIN);
        prims.insert(static_symbol!("__prim_cos"), INSTR_COS);
        prims.insert(static_symbol!("__prim_tan"), INSTR_TAN);
        prims.insert(static_symbol!("__prim_asin"), INSTR_ASIN);
        prims.insert(static_symbol!("__prim_acos"), INSTR_ACOS);
        prims.insert(static_symbol!("__prim_atan"), INSTR_ATAN);
        prims.insert(static_symbol!("__prim_ceiling"), INSTR_CEILING);
        prims.insert(static_symbol!("__prim_floor"), INSTR_FLOOR);
        prims.insert(static_symbol!("__prim_round"), INSTR_ROUND);
        prims.insert(static_symbol!("__prim_log"), INSTR_LOG);
        prims.insert(static_symbol!("__prim_exp"), INSTR_EXP);
        prims.insert(static_symbol!("__prim_to_int"), INSTR_TOINT);
        prims.insert(static_symbol!("_prim_array_ref"), INSTR_ARRAY_REF);
        prims.insert(static_symbol!("_prim_array_set"), INSTR_ARRAY_SET);
        prims.insert(static_symbol!("_prim_array_create"), INSTR_ALLOC_ARRAY);
        prims.insert(static_symbol!("_prim_array_size"), INSTR_ARRAY_SIZE);
        prims.insert(static_symbol!("_prim_bytes_ref"), INSTR_BYTES_REF);
        prims.insert(static_symbol!("_prim_bytes_set"), INSTR_BYTES_SET);
        prims.insert(static_symbol!("_prim_bytes_create"), INSTR_ALLOC_BYTES); 
        prims.insert(static_symbol!("_prim_bytes_size"), INSTR_BYTES_SIZE);
        prims.insert(static_symbol!("_prim_table_create"), INSTR_ALLOC_TABLE);
        prims.insert(static_symbol!("_prim_table_set"), INSTR_TABLE_SET);
        prims.insert(static_symbol!("_prim_table_size"), INSTR_TABLE_SIZE);
        prims.insert(static_symbol!("_prim_table_get"), INSTR_TABLE_GET);
        prims.insert(static_symbol!("_prim_table_delete"), INSTR_TABLE_DELETE);
        prims 
    });
    BYTECODE_PRIMS.get(prim).copied()
}



