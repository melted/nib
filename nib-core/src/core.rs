use symbol_table::static_symbol;

use crate::{
    ast::{self, ExpressionNode, Literal, Pattern, PatternNode},
    common::{Error, Metadata, Name, Node, Result, Symbol, sym},
};
use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
    mem,
    sync::LazyLock,
};

mod tests;

pub fn desugar(module: ast::Module) -> Result<Module> {
    let mut state = DesugarState::new(module.metadata);
    let mut to_desugar = Vec::new();
    for d in module.declarations {
        match d {
            ast::Declaration::Binding(bind) => {
                to_desugar.push(bind);
            }
        }
    }
    state.desugar_bindings(&to_desugar, false)?;
    let mut locals = HashSet::new();
    for b in &state.bindings {
        if let Binder::Local(l) = &b.binder {
            locals.insert(l.clone());
        }
    }

    Ok(Module {
        metadata: state.metadata,
        bindings: state.bindings,
        locals,
    })
}

pub fn desugar_expression(expr: ExpressionNode) -> Result<Expression> {
    let meta = Metadata::empty();
    let mut state = DesugarState::new(meta);
    let expression = state.desugar_expression(&expr)?;
    if !state.bindings.is_empty() {
        Ok(Expression::Where(
            expr.id,
            Box::new(expression),
            state.bindings,
        ))
    } else {
        Ok(expression)
    }
}

struct DesugarState {
    bindings: Vec<Binding>,
    metadata: Metadata,
    current_bindings: BTreeSet<Binder>,
    last_local: u32,
    last_arg: u32,
}

impl DesugarState {
    fn new(metadata: Metadata) -> Self {
        DesugarState {
            bindings: Vec::new(),
            metadata,
            current_bindings: BTreeSet::new(),
            last_local: 0,
            last_arg: 0,
        }
    }

    fn desugar_bindings(&mut self, bindings: &[ast::Binding], is_local: bool) -> Result<()> {
        for b in bindings {
            let names = b.bound_names();
            for n in names {
                let binder = self.desugar_binding_name(&n, is_local)?;
                self.current_bindings.insert(binder);
            }
        }
        for b in bindings {
            self.desugar_binding(b, is_local)?;
        }
        Ok(())
    }

    fn desugar_binding(&mut self, binding: &ast::Binding, is_local: bool) -> Result<()> {
        match binding {
            ast::Binding::FunBinding(fb) => self.desugar_funbinding(fb, is_local),
            ast::Binding::OpBinding(ob) => self.desugar_opbinding(ob, is_local),
            ast::Binding::VarBinding(vb) => self.desugar_varbinding(vb, is_local),
        }
    }

    fn desugar_funbinding(&mut self, ast_binding: &ast::FunBinding, is_local: bool) -> Result<()> {
        let binder = self.desugar_binding_name(&ast_binding.name, is_local)?;
        self.metadata.last_id += 1;
        let result = self.desugar_funclauses(self.metadata.last_id, &ast_binding.clauses)?;
        let bind = Binding::make(
            ast_binding.id,
            binder,
            Expression::Function(Box::new(result)),
        );
        self.bindings.push(bind);
        Ok(())
    }

    fn desugar_opbinding(&mut self, ast_binding: &ast::OpBinding, is_local: bool) -> Result<()> {
        let name = ast_binding.op.to_name(); // No desugar. Operators can't be used qualified,
        let funclauses: Vec<_> = ast_binding.clauses.iter().map(to_funclause).collect();
        self.desugar_funbinding(
            &ast::FunBinding {
                id: ast_binding.id,
                name,
                clauses: funclauses,
            },
            is_local,
        )
    }

    fn desugar_varbinding(&mut self, ast_binding: &ast::VarBinding, is_local: bool) -> Result<()> {
        let pat = &ast_binding.lhs;
        let rhs = self.desugar_expression(&ast_binding.rhs)?;
        match &pat.pattern {
            Pattern::Var(v) => {
                let binder = self.desugar_binding_name(v, is_local)?;
                self.bindings
                    .push(Binding::make(ast_binding.id, binder, rhs));
            }
            Pattern::Wildcard => {
                self.bindings
                    .push(Binding::make(ast_binding.id, Binder::Unbound, rhs));
            }
            _ => {
                let mut replaced = HashMap::new();
                let mut counter = 0;
                let p = Self::pattern_with_plain_vars(&pat, &mut counter, &mut replaced);
                let parts = self.desugar_arg_pattern(&p, &rhs)?;
                let vars = p.bound_vars().into_iter().map(|n| (&n).into());
                let mut var_exp = vec![var(&sym("_prim_array_make"))];
                let mut var_syms = Vec::new();
                for v in vars {
                    var_exp.push(var(&v));
                    var_syms.push(replaced[&v].clone());
                }
                let on_fail = app(&[
                    var(&sym("_prim_panic")),
                    literal(&Literal::String(
                        "Failure to match irrefutable varbinding".to_string(),
                    )),
                ]);
                let (is_irrefutable, pexpr) =
                    self.build_pattern_expression(&parts, &app(&var_exp), &on_fail)?;
                let nam_arr = self.next_local();
                let binding = Binding::make(ast_binding.id, Binder::Local(nam_arr.clone()), pexpr);
                self.bindings.push(binding);
                for (i, k) in var_syms.iter().enumerate() {
                    let rhs = Expression::App(
                        self.new_id(),
                        vec![
                            self.named_var("_prim_array_ref"),
                            Expression::Literal(self.new_id(), Literal::Integer(i as i64)),
                            self.named_var(&nam_arr.string()),
                        ],
                    );
                    let binder = self.desugar_binding_name(k, is_local)?;
                    let bind = Binding::make(self.new_id(), binder, rhs);
                    self.bindings.push(bind);
                }
            }
        }
        Ok(())
    }

    fn desugar_binding_name(&mut self, name: &Name, is_local: bool) -> Result<Binder> {
        if is_local {
            Ok(Binder::Local(name.clone()))
        } else if name.top() == sym("local") {
            let slice = name.tail();
            Ok(Binder::Local(Name::try_from(&slice)?))
        } else {
            Ok(Binder::Public(name.clone()))
        }
    }

    fn desugar_arg_pattern(
        &mut self,
        pattern: &PatternNode,
        expr: &Expression,
    ) -> Result<Vec<PatternParts>> {
        let bound_names = pattern.bound_vars();
        match &pattern.pattern {
            Pattern::Alias(pat, alias) => {
                let Name::Plain(name) = alias else {
                    return self.error("Qualified name in alias pattern.");
                };
                let mut parts = Vec::new();
                parts.push(PatternParts::Bind(*name, expr.to_owned()));
                let mut inner = self.desugar_arg_pattern(pat, &var(name))?;
                parts.append(&mut inner);
                Ok(parts)
            }
            Pattern::Custom(name, fields) => {
                let mut parts = Vec::new();
                let loc = self.non_shadowed_var(expr);
                parts.push(PatternParts::Bind(
                    loc,
                    Expression::App(pattern.id, vec![name_expr(name), expr.clone()]),
                ));
                let failed = app(&[
                    var(&static_symbol!("_prim_neq")),
                    var(&loc),
                    literal(&Literal::Bool(false)),
                ]);
                parts.push(PatternParts::Check(failed));
                let size = app(&[var(&static_symbol!("_prim_array_size")), var(&loc)]);
                let size_check = app(&[
                    var(&static_symbol!("_prim_eq")),
                    size,
                    literal(&Literal::Integer(fields.len() as i64)),
                ]);
                parts.push(PatternParts::Check(size_check));
                for (i, f) in fields.iter().enumerate() {
                    let refer = app(&[
                        var(&static_symbol!("_prim_array_ref")),
                        literal(&Literal::Integer(i as i64)),
                        var(&loc),
                    ]);
                    let mut field = self.desugar_arg_pattern(f, &refer)?;
                    parts.append(&mut field);
                }
                Ok(parts)
            }
            Pattern::Array(fields) => self.desugar_arg_pattern(
                &PatternNode {
                    id: 0,
                    pattern: Pattern::Custom(Name::str("_prim_array_match"), fields.to_vec()),
                },
                expr,
            ),
            Pattern::Ellipsis(name) => {
                if let Some(n) = name {
                    let Name::Plain(s) = n else {
                        return self.error("Qualified name in ellipsis pattern.");
                    };
                    Ok(vec![PatternParts::Bind(*s, expr.clone())])
                } else {
                    Ok(vec![])
                }
            }
            Pattern::Literal(lit) => {
                // TODO: Will this work for bytes?
                let check = app(&[var(&static_symbol!("_prim_eq")), expr.clone(), literal(lit)]);
                Ok(vec![PatternParts::Check(check)])
            }
            Pattern::Typed(pat, typ) => {
                let mut inner = self.desugar_arg_pattern(pat, expr)?;
                let mut val = expr.clone();
                if let Some(PatternParts::Bind(name, exp)) = inner.last()
                    && exp == expr
                {
                    val = var(name);
                }
                let get_type = app(&[var(&static_symbol!("_prim_type")), val]);
                let check = app(&[var(&static_symbol!("_prim_eq")), get_type, name_expr(typ)]);
                inner.push(PatternParts::Check(check));
                Ok(inner)
            }
            Pattern::Var(Name::Plain(name)) => Ok(vec![PatternParts::Bind(*name, expr.clone())]),
            Pattern::Var(_) => self.error("Qualified name in arg pattern"),
            Pattern::Wildcard => Ok(vec![]),
        }
    }

    fn desugar_expression(&mut self, expression: &ExpressionNode) -> Result<Expression> {
        match &expression.expr {
            ast::Expression::App(x) => {
                let mut args = Vec::new();
                for a in x {
                    args.push(self.desugar_expression(a)?);
                }
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Array(v) => {
                let mut args = Vec::new();
                args.push(self.named_var("_prim_array_make"));
                for a in v {
                    args.push(self.desugar_expression(a)?);
                }
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Binop(ast::Binop { op, lhs, rhs }) => {
                let args = vec![
                    self.var(op.to_name()),
                    self.desugar_expression(lhs)?,
                    self.desugar_expression(rhs)?,
                ];
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Cond(cond) => {
                let pred = self.desugar_expression(&cond.pred)?;
                let if_true = self.desugar_expression(&cond.on_true)?;
                let if_false = self.desugar_expression(&cond.on_false)?;
                Ok(Expression::Cond(
                    expression.id,
                    Box::new(Cond::new(pred, if_true, if_false)),
                ))
            }
            ast::Expression::Lambda(funs) => {
                let function = self.desugar_funclauses(expression.id, funs)?;
                Ok(fun(function))
            }
            ast::Expression::Literal(lit) => Ok(Expression::Literal(expression.id, lit.clone())),
            ast::Expression::Projection(projs) => {
                let mut args = Vec::new();
                args.push(self.named_var("_prim_project"));
                for a in projs {
                    args.push(self.desugar_expression(a)?);
                }
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Var(Name::Plain(n)) => Ok(var(n)),
            ast::Expression::Where(exp, ast_bindings) => {
                let mut new_locals = self.current_bindings.clone();
                let mut binds = Vec::new();
                mem::swap(&mut binds, &mut self.bindings);
                mem::swap(&mut new_locals, &mut self.current_bindings);
                self.desugar_bindings(ast_bindings, true)?;
                let lhs = self.desugar_expression(exp)?;
                mem::swap(&mut new_locals, &mut self.current_bindings);
                mem::swap(&mut binds, &mut self.bindings);
                Ok(Expression::Where(expression.id, Box::new(lhs), binds))
            }
            _ => self.error(&format!("couldn't desugar {}", expression)),
        }
    }

    fn set_captured_vars(&self, function: &mut Function) -> Result<()> {
        let free = free_vars(function)?;
        let current_locals = self.current_bindings.iter().filter_map(|b| match b {
            Binder::Local(l) => Some(Symbol::from(l)),
            _ => None,
        });
        function.captures = free
            .intersection(&current_locals.collect())
            .copied()
            .collect();
        Ok(())
    }

    fn no_match_expression() -> Expression {
        app(&[
            var(&static_symbol!("_prim_panic")),
            literal(&Literal::String("No matching pattern".to_owned())),
        ])
    }

    fn desugar_funclauses(&mut self, id: Node, clauses: &[ast::FunClause]) -> Result<Function> {
        let arity = verify_arity(clauses)?;
        let mut exp = None;
        let args = self.args(&arity);
        let mut old_current_bindings = self.current_bindings.clone();
        self.current_bindings
            .extend(args.iter().map(|a| Binder::Local(Name::from(*a))));
        for (i, c) in clauses.iter().enumerate() {
            let fail_exp = if i + 1 < clauses.len() {
                app(&[var(&local(i + 1)), nil()])
            } else {
                Self::no_match_expression()
            };
            let (is_irrefutable, next_exp) = self.desugar_funclause(c, &args, &fail_exp)?;
            if let Some(e) = &mut exp {
                let mut function = Function::new(&[sym("$_")], &Arity::Fixed(1), &next_exp);
                self.set_captured_vars(&mut function)?;
                let name = Name::from(local(i));
                self.current_bindings.insert(Binder::Local(name.clone()));
                let bind = Binding::make(0, Binder::Local(name), fun(function));
                match e {
                    Expression::Where(_, exp, bindings) => {
                        bindings.insert(0, bind);
                    }
                    _ => {
                        exp = Some(Expression::Where(0, Box::new(e.clone()), vec![bind]));
                    }
                }
            } else {
                exp = Some(next_exp)
            }
            if is_irrefutable {
                break;
            }
        }
        mem::swap(&mut self.current_bindings, &mut old_current_bindings);
        let mut function = Function::new(&args, &arity, &exp.unwrap());
        self.set_captured_vars(&mut function)?;
        Ok(function)
    }

    fn desugar_funclause(
        &mut self,
        clause: &ast::FunClause,
        args: &[Symbol],
        on_fail: &Expression,
    ) -> Result<(bool, Expression)> {
        let mut all_parts = Vec::new();
        for (p, v) in clause.args.iter().zip(args.iter()) {
            let arg_exp = Expression::Var(0, *v);
            let mut parts = self.desugar_arg_pattern(p, &arg_exp)?;
            all_parts.append(&mut parts);
        }
        if let Some(guard) = &clause.guard {
            let guard_exp = self.desugar_expression(guard)?;
            all_parts.push(PatternParts::Check(guard_exp));
        }
        let exp = self.desugar_expression(&clause.body)?;
        self.build_pattern_expression(&all_parts, &exp, on_fail)
    }

    fn build_pattern_expression(
        &mut self,
        all_parts: &[PatternParts],
        base: &Expression,
        on_fail: &Expression,
    ) -> Result<(bool, Expression)> {
        let mut exp = base.clone();
        let is_irrefutable = all_parts
            .iter()
            .all(|p| !matches!(p, PatternParts::Check(_)));
        for p in all_parts.iter().rev() {
            match p {
                PatternParts::Check(pred) => {
                    exp = cond(pred, &exp, on_fail);
                }
                PatternParts::Bind(var, expression) => {
                    if let Expression::Var(n, v) = expression {
                        // Just a = b, can rename all 'a' to 'b' instead of
                        // creating a binding.
                        exp = rename(&exp, var, v);
                    } else {
                        let binding =
                            Binding::make(0, Binder::Local(Name::sym(var)), expression.clone());
                        match &mut exp {
                            Expression::Where(n, expr, binds) => {
                                binds.insert(0, binding);
                            }
                            _ => {
                                exp = Expression::Where(0, Box::new(exp.clone()), vec![binding]);
                            }
                        }
                    }
                }
            }
        }
        Ok((is_irrefutable, exp))
    }

    fn pattern_with_plain_vars(
        pattern: &PatternNode,
        counter: &mut i32,
        replacements: &mut HashMap<Symbol, Name>,
    ) -> PatternNode {
        let mut p = pattern.clone();
        match &pattern.pattern {
            Pattern::Ellipsis(Some(old)) => {
                let n = Symbol::from(format!("$z{}", counter));
                *counter += 1;
                replacements.insert(n, old.clone());
                p.pattern = Pattern::Ellipsis(Some(Name::Plain(n)));
            }
            Pattern::Var(old) => {
                let n = Symbol::from(format!("$z{}", counter));
                *counter += 1;

                replacements.insert(n, old.clone());
                p.pattern = Pattern::Var(Name::Plain(n))
            }
            Pattern::Array(arr) => {
                let mut new_arr = Vec::new();
                for p in arr.iter() {
                    new_arr.push(Self::pattern_with_plain_vars(p, counter, replacements));
                }
                p.pattern = Pattern::Array(new_arr);
            }
            Pattern::Alias(pat, old) => {
                let n = Symbol::from(format!("$z{}", counter));
                *counter += 1;
                replacements.insert(n, old.clone());
                let new_pat = Self::pattern_with_plain_vars(pat, counter, replacements);
                p.pattern = Pattern::Alias(Box::new(new_pat), Name::Plain(n));
            }
            Pattern::Custom(matcher, fields) => {
                let mut new_fields = Vec::new();
                for p in fields.iter() {
                    new_fields.push(Self::pattern_with_plain_vars(p, counter, replacements));
                }
                p.pattern = Pattern::Custom(matcher.clone(), new_fields);
            }
            Pattern::Typed(pat, t) => {
                let new_pat = Self::pattern_with_plain_vars(pat, counter, replacements);
                p.pattern = Pattern::Typed(Box::new(new_pat), t.clone());
            }
            _ => {}
        }
        p
    }

    fn next_local(&mut self) -> Name {
        self.last_local += 1;
        let id = format!("$local{}", self.last_local);
        Name::Plain(Symbol::from(id))
    }

    fn next_lambda(&mut self) -> Name {
        self.last_local += 1;
        let id = format!("$lambda{}", self.last_local);
        Name::Plain(Symbol::from(id))
    }

    fn next_arg(&mut self) -> Symbol {
        self.last_arg += 1;
        Symbol::from(format!("$arg{}", self.last_arg))
    }

    fn non_shadowed_var(&mut self, exp: &Expression) -> Symbol {
        let mut vars = HashSet::new();
        let mut locals = HashMap::new();
        free_vars_expression(exp, &mut vars, &mut locals);
        let mut counter = 0;
        loop {
            let next = Symbol::from(format!("x{}", counter));
            if !vars.contains(&next) {
                return Symbol::from(next);
            }
            counter += 1;
        }
    }

    fn args(&mut self, arity: &Arity) -> Vec<Symbol> {
        let n = match arity {
            Arity::Fixed(n) => *n as usize,
            Arity::VarArg(n, _) => (*n + 1) as usize,
        };
        let mut args = Vec::new();
        for i in 0..n {
            args.push(self.next_arg());
        }
        args
    }

    fn new_id(&mut self) -> u32 {
        self.metadata.last_id += 1;
        self.metadata.last_id
    }

    fn named_var(&mut self, name: &str) -> Expression {
        Expression::Var(self.new_id(), Symbol::from(name))
    }

    fn var(&mut self, name: Name) -> Expression {
        Expression::Var(self.new_id(), Symbol::from(name.top()))
    }

    fn error<T>(&self, msg: &str) -> Result<T> {
        Err(Error::Desugar {
            msg: msg.to_owned(),
            loc: None,
        })
    }
}

fn to_funclause(op_clause: &ast::OpClause) -> ast::FunClause {
    ast::FunClause {
        id: op_clause.id,
        args: vec![op_clause.lpat.clone(), op_clause.rpat.clone()],
        guard: op_clause.guard.clone(),
        body: op_clause.body.clone(),
    }
}

fn verify_arity(clauses: &[ast::FunClause]) -> Result<Arity> {
    let arity = get_arity(&clauses[0].args);
    for c in clauses[1..].iter() {
        let next = get_arity(&c.args);
        if arity != next {
            return Err(Error::runtime_error(
                "Function clauses must have same arity",
            ));
        }
    }
    Ok(arity)
}

pub fn get_arity(patterns: &[PatternNode]) -> Arity {
    let mut vararg = false;
    let mut index = 0;
    for (i, p) in patterns.iter().enumerate() {
        if p.is_ellipsis() {
            vararg = true;
            index = i;
        }
    }
    let len = patterns.len() as u32;
    if vararg {
        Arity::VarArg(len, index as u32)
    } else {
        Arity::Fixed(len)
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub metadata: Metadata,
    pub bindings: Vec<Binding>,
    pub locals: HashSet<Name>,
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self.metadata)?;
        for b in &self.bindings {
            writeln!(f, "{}", b)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub id: Node,
    pub binder: Binder,
    pub body: Expression,
}

impl Binding {
    pub fn make(id: Node, binder: Binder, body: Expression) -> Self {
        Binding { id, binder, body }
    }

    pub fn name(&self) -> Option<Name> {
        match &self.binder {
            Binder::Local(n) | Binder::Public(n) => Some(n.clone()),
            Binder::Unbound => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Binder {
    Public(Name),
    Local(Name),
    Unbound,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub code_ref: Symbol,
    pub args: Vec<Symbol>,
    pub captures: Vec<Symbol>,
    pub code_captures: Vec<Symbol>,
    pub literal_captures: Vec<Literal>,
    pub arity: Arity,
    pub body: Expression,
}

impl Function {
    pub fn new(args: &[Symbol], arity: &Arity, expression: &Expression) -> Self {
        let mut captures = HashSet::new();
        code_captures(expression, &mut captures);
        let mut lits = HashSet::new();
        literal_captures(expression, &mut lits);
        Function {
            code_ref: next_code_id(),
            args: args.to_vec(),
            captures: Vec::new(),
            code_captures: captures.into_iter().collect::<Vec<_>>(),
            literal_captures: lits.into_iter().collect(),
            arity: arity.clone(),
            body: expression.clone(),
        }
    }
}

fn code_captures(expression: &Expression, captures: &mut HashSet<Symbol>) {
    match expression {
        Expression::Cond(_, cond) => {
            code_captures(&cond.pred, captures);
            code_captures(&cond.if_true, captures);
            code_captures(&cond.if_false, captures);
        }
        Expression::App(_, expressions) => {
            for e in expressions {
                code_captures(e, captures);
            }
        }
        Expression::Function(function) => {
            captures.insert(function.code_ref);
            for c in &function.code_captures {
                captures.insert(*c);
            }
        }
        Expression::Where(_, expression, bindings) => {
            code_captures(expression, captures);
            for b in bindings {
                code_captures(&b.body, captures);
            }
        }
        _ => {}
    }
}

fn literal_captures(expression: &Expression, captures: &mut HashSet<Literal>) {
    match expression {
        Expression::Cond(_, cond) => {
            literal_captures(&cond.pred, captures);
            literal_captures(&cond.if_true, captures);
            literal_captures(&cond.if_false, captures);
        }
        Expression::App(_, expressions) => {
            for e in expressions {
                literal_captures(e, captures);
            }
        }
        Expression::Function(function) => {
            for lit in &function.literal_captures {
                captures.insert(lit.clone());
            }
        }
        Expression::Where(_, expression, bindings) => {
            literal_captures(expression, captures);
            for b in bindings {
                literal_captures(&b.body, captures);
            }
        }
        Expression::Literal(_, lit) => match lit {
            Literal::Bytearray(_) | Literal::String(_) => {
                captures.insert(lit.clone());
            }
            _ => {}
        },
        _ => {}
    }
}

pub fn next_code_id() -> Symbol {
    unsafe {
        static mut LOCAL_VAL: LazyLock<u32> = LazyLock::new(|| 0);
        *LOCAL_VAL += 1;
        sym(&format!("$code{}", *LOCAL_VAL))
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        let ellipsis = match self.arity {
            Arity::Fixed(_) => None,
            Arity::VarArg(_, n) => Some(n as usize),
        };
        for (i, a) in self.args.iter().enumerate() {
            if let Some(n) = ellipsis
                && n == i
            {
                write!(f, "...")?;
            }
            write!(f, "{} ", a)?;
        }
        write!(f, "-> {}", self.body)?;
        write!(f, " }}")
    }
}

impl Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {})", self.binder, self.body)
    }
}

impl Display for Binder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Binder::Public(name) => write!(f, "define {} ", name),
            Binder::Local(s) => write!(f, "local {} ", s),
            Binder::Unbound => write!(f, "do "),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cond {
    pub pred: Expression,
    pub if_true: Expression,
    pub if_false: Expression,
}

impl Cond {
    pub fn new(pred: Expression, if_true: Expression, if_false: Expression) -> Self {
        Cond {
            pred,
            if_true,
            if_false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Node, Literal),
    Var(Node, Symbol),
    Cond(Node, Box<Cond>),
    App(Node, Vec<Expression>),
    Function(Box<Function>),
    Where(Node, Box<Expression>, Vec<Binding>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::App(_, exprs) => {
                write!(f, "(")?;
                write!(f, "{}", exprs[0])?;
                for e in exprs[1..].iter() {
                    write!(f, " {}", e)?;
                }
                write!(f, ")")?;
            }
            Expression::Cond(_, cond) => {
                write!(f, "({} => {} ; {})", cond.pred, cond.if_true, cond.if_false)?;
            }
            Expression::Literal(_, lit) => {
                write!(f, "{}", lit)?;
            }
            Expression::Var(_, v) => {
                write!(f, "{}", v)?;
            }
            Expression::Function(fun) => {
                write!(f, "{}", fun)?;
            }
            Expression::Where(_, lhs, binds) => {
                write!(f, "({} where ", lhs)?;
                for b in binds {
                    write!(f, "{}", b)?;
                }
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}

fn var(s: &Symbol) -> Expression {
    Expression::Var(0, *s)
}

fn name_expr(n: &Name) -> Expression {
    match n {
        Name::Plain(s) => var(s),
        Name::Qualified(p, s) => {
            let t = n.top();
            let rest = p[1..].iter().chain(vec![s]);
            let mut args = vec![var(&Symbol::from("_prim_project")), var(&t)];
            for a in rest {
                args.push(literal(&Literal::Symbol(*a)));
            }
            app(&args)
        }
    }
}

fn local(n: usize) -> Symbol {
    Symbol::from(format!("$loc{}", n))
}

fn app(exps: &[Expression]) -> Expression {
    Expression::App(0, exps.to_vec())
}

fn fun(f: Function) -> Expression {
    Expression::Function(Box::new(f))
}

fn literal(lit: &Literal) -> Expression {
    Expression::Literal(0, lit.clone())
}

fn nil() -> Expression {
    literal(&Literal::Nil)
}

fn cond(pred: &Expression, if_true: &Expression, if_false: &Expression) -> Expression {
    let cond = Cond::new(pred.clone(), if_true.clone(), if_false.clone());
    Expression::Cond(0, Box::new(cond))
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternParts {
    Check(Expression),
    Bind(Symbol, Expression),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub enum Arity {
    Fixed(u32),
    VarArg(u32, u32),
}

impl Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arity::Fixed(n) => write!(f, "{}", n),
            Arity::VarArg(n, m) => write!(f, "{}({})+", n, m),
        }
    }
}

impl Arity {
    pub fn min_arity(&self) -> u32 {
        match self {
            Arity::Fixed(n) | Arity::VarArg(n, _) => *n,
        }
    }
}

pub fn rename(expr: &Expression, old_name: &Symbol, new_name: &Symbol) -> Expression {
    match expr {
        Expression::Var(n, v) if v == old_name => Expression::Var(*n, *new_name),
        Expression::App(n, exps) => {
            let mut new_exps = Vec::new();
            for e in exps {
                new_exps.push(rename(e, old_name, new_name));
            }
            Expression::App(*n, new_exps)
        }
        Expression::Cond(n, cond) => {
            let new_cond = Cond::new(
                rename(&cond.pred, old_name, new_name),
                rename(&cond.if_true, old_name, new_name),
                rename(&cond.if_false, old_name, new_name),
            );
            Expression::Cond(*n, Box::new(new_cond))
        }
        Expression::Function(old_function) => {
            let new_body = if !old_function.args.contains(old_name) {
                rename(&old_function.body, old_name, new_name)
            } else {
                old_function.body.clone()
            };
            let mut new_function =
                Function::new(&old_function.args, &old_function.arity, &new_body);
            new_function.captures = old_function.captures.clone();
            fun(new_function)
        }
        Expression::Where(n, exp, defs) => {
            let mut shadowed = false;
            let mut new_defs = Vec::new();
            for b in defs {
                let new_body = if !shadowed {
                    rename(&b.body, old_name, new_name)
                } else {
                    b.body.clone()
                };
                let mut new_binding = b.clone();
                new_binding.body = new_body;
                new_defs.push(new_binding);
                if let Some(Name::Plain(x)) = &b.name()
                    && old_name == x
                {
                    shadowed = true;
                }
            }
            let new_exp = if !shadowed {
                Box::new(rename(exp, old_name, new_name))
            } else {
                exp.clone()
            };
            Expression::Where(*n, new_exp, new_defs)
        }
        x => x.clone(),
    }
}

pub fn free_vars(function: &Function) -> Result<HashSet<Symbol>> {
    let mut vars = HashSet::new();
    let mut locals = HashMap::new();
    add_locals(&mut locals, &function.args);
    free_vars_expression(&function.body, &mut vars, &mut locals);
    remove_locals(&mut locals, &function.args);
    Ok(vars)
}

fn add_local(locals: &mut HashMap<Symbol, i32>, var: &Symbol) {
    locals.insert(*var, locals.get(var).unwrap_or(&0) + 1);
}

fn remove_local(locals: &mut HashMap<Symbol, i32>, var: &Symbol) {
    if let Some(v) = locals.get_mut(var) {
        *v -= 1;
        if *v == 0 {
            locals.remove(var);
        }
    }
}

fn add_locals(locals: &mut HashMap<Symbol, i32>, vars: &[Symbol]) {
    for v in vars {
        add_local(locals, v);
    }
}

fn remove_locals(locals: &mut HashMap<Symbol, i32>, vars: &[Symbol]) {
    for v in vars {
        remove_local(locals, v);
    }
}

pub fn free_vars_expression(
    expr: &Expression,
    vars: &mut HashSet<Symbol>,
    locals: &mut HashMap<Symbol, i32>,
) {
    match expr {
        Expression::Literal(_, literal) => {}
        Expression::Var(_, v) => {
            if locals.get(v).is_none() {
                vars.insert(*v);
            }
        }
        Expression::Cond(_, cond) => {
            free_vars_expression(&cond.pred, vars, locals);
            free_vars_expression(&cond.if_false, vars, locals);
            free_vars_expression(&cond.if_true, vars, locals);
        }
        Expression::App(_, expressions) => {
            for e in expressions {
                free_vars_expression(e, vars, locals);
            }
        }
        Expression::Function(function) => {
            add_locals(locals, &function.args);
            free_vars_expression(&function.body, vars, locals);
            remove_locals(locals, &function.args);
        }
        Expression::Where(_, expression, bindings) => {
            let mut used = HashSet::new();
            let mut bound = Vec::new();
            for b in bindings {
                match &b.binder {
                    Binder::Local(n) | Binder::Public(n) => {
                        let var = Symbol::from(n.top());
                        bound.push(var);
                        add_local(locals, &var);
                    }
                    _ => {}
                };
                free_vars_expression(&b.body, &mut used, locals);
            }
            free_vars_expression(expression, &mut used, locals);
            remove_locals(locals, &bound);
            for v in used {
                vars.insert(v.to_owned());
            }
        }
    }
}
