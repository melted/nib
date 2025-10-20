use crate::ast::{ExpressionNode, Pattern};
use crate::common::Symbol;
use crate::{
    ast::{self, Literal, PatternNode},
    common::{Error, Metadata, Name, Node, Result},
};
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

mod tests;

pub fn desugar(module: ast::Module) -> Result<Module> {
    let mut state = DesugarState::new(module.metadata);
    state.module_name = state.metadata.base_name.clone();
    for d in module.declarations {
        match d {
            ast::Declaration::Use(ud) => {
                state.metadata.using.insert(ud.name);
            }
            ast::Declaration::Module(md) => {
                state.module_name = Some(md.name);
            }
            ast::Declaration::Binding(bind) => {
                let mut b = state.desugar_binding(&bind, false)?;
                state.bindings.append(&mut b);
            }
        }
    }
    Ok(Module {
        metadata: state.metadata,
        bindings: state.bindings,
    })
}

pub fn desugar_expression(expr: ExpressionNode) -> Result<Expression> {
    let meta = Metadata::new(None);
    let mut state = DesugarState::new(meta);
    state.desugar_expression(&expr)
}

struct DesugarState {
    module_name: Option<Name>,
    bindings: Vec<Binding>,
    metadata: Metadata,
    last_local: u32,
    last_arg: u32,
}

impl DesugarState {
    fn new(metadata: Metadata) -> Self {
        DesugarState {
            module_name: None,
            bindings: Vec::new(),
            metadata,
            last_local: 0,
            last_arg: 0,
        }
    }

    fn desugar_binding(&mut self, binding: &ast::Binding, is_local: bool) -> Result<Vec<Binding>> {
        match binding {
            ast::Binding::FunBinding(fb) => self.desugar_funbinding(fb, is_local),
            ast::Binding::OpBinding(ob) => self.desugar_opbinding(ob, is_local),
            ast::Binding::VarBinding(vb) => self.desugar_varbinding(vb, is_local),
        }
    }

    fn desugar_funbinding(
        &mut self,
        ast_binding: &ast::FunBinding,
        is_local: bool,
    ) -> Result<Vec<Binding>> {
        let binder = if is_local {
            Binder::Local(ast_binding.name.clone())
        } else {
            Binder::Public(self.desugar_binding_name(&ast_binding.name)?)
        };
        self.metadata.last_id += 1;
        let result = self.desugar_funclauses(self.metadata.last_id, &ast_binding.clauses)?;
        Ok(vec![Binding::binding(ast_binding.id, binder, result)])
    }

    fn desugar_opbinding(
        &mut self,
        ast_binding: &ast::OpBinding,
        is_local: bool,
    ) -> Result<Vec<Binding>> {
        let name = ast_binding.op.to_name(); // No desugar. Operators can't be used qualified,
        let funclauses: Vec<_> = ast_binding
            .clauses
            .iter()
            .map(|oc| to_funclause(oc))
            .collect();
        self.metadata.last_id += 1;
        let result = self.desugar_funclauses(self.metadata.last_id, &funclauses)?;
        Ok(vec![Binding::binding(
            ast_binding.id,
            if is_local {
                Binder::Local(name)
            } else {
                Binder::Public(name)
            },
            result,
        )])
    }

    fn desugar_varbinding(
        &mut self,
        ast_binding: &ast::VarBinding,
        is_local: bool,
    ) -> Result<Vec<Binding>> {
        let mut pat = &ast_binding.lhs;
        let rhs = self.desugar_expression(&ast_binding.rhs)?;
        match &pat.pattern {
            Pattern::Var(v) => {
                let binder = if is_local {
                    Binder::Local(v.clone())
                } else {
                    Binder::Public(self.desugar_binding_name(&v)?)
                };
                Ok(vec![Binding::binding(ast_binding.id, binder, rhs)])
            }
            Pattern::Wildcard => Ok(vec![Binding::binding(ast_binding.id, Binder::Unbound, rhs)]),
            _ => {
                let mut counter = 0;
                let mut replacements = HashMap::new();
                let fun_pat = self.pattern_with_plain_vars(pat, &mut counter, &mut replacements);
                let fun_names = replacements.keys().collect::<Vec<_>>();
                let v = fun_names
                    .iter()
                    .map(|&n| ExpressionNode::from(ast::Expression::Var(n.clone())));
                let arr_exp = ExpressionNode::from(ast::Expression::Array(v.collect()));
                let panic_exp = ExpressionNode::from(ast::Expression::App(vec![
                    ExpressionNode::from(ast::Expression::Var(Name::str("_prim_panic"))),
                    ExpressionNode::from(ast::Expression::Literal(Literal::String(
                        "irrefutable var pattern mismatch".to_owned(),
                    ))),
                ]));
                let clause = ast::FunClause {
                    id: 0,
                    args: vec![fun_pat.clone()],
                    guard: None,
                    body: arr_exp,
                };
                let fallback = ast::FunClause {
                    id: 0,
                    args: vec![ast::PatternNode::from(Pattern::Wildcard)],
                    guard: None,
                    body: panic_exp,
                };
                let fun = ExpressionNode::from(ast::Expression::Lambda(vec![clause, fallback]));
                let expr =
                    ExpressionNode::from(ast::Expression::App(vec![fun, ast_binding.rhs.clone()]));
                let desugared = self.desugar_expression(&expr)?;
                let nam_arr = self.next_local();
                let binding =
                    Binding::binding(ast_binding.id, Binder::Local(nam_arr.clone()), desugared);
                let mut bindings = vec![binding];
                for (i, k) in fun_names.iter().enumerate() {
                    let old = replacements.get(k).unwrap();
                    let rhs = Expression::App(
                        self.new_id(),
                        vec![
                            self.named_var("_prim_array_ref"),
                            self.named_var(&nam_arr.string()),
                            Expression::Literal(self.new_id(), Literal::Integer(i as i64)),
                        ],
                    );
                    let binder = if is_local {
                        Binder::Local(old.clone())
                    } else {
                        Binder::Public(self.desugar_binding_name(old)?)
                    };
                    let bind = Binding::binding(self.new_id(), binder, rhs);
                    bindings.push(bind);
                }
                Ok(bindings)
            }
        }
    }

    fn desugar_binding_name(&mut self, name: &Name) -> Result<Name> {
        match (&self.module_name, name) {
            (None, _) | (_, Name::Qualified(_, _)) => Ok(name.clone()),
            (Some(mod_name), Name::Plain(_)) => Ok(Name::append(mod_name, name)?),
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
                parts.push(PatternParts::Bind(name.clone(), expr.to_owned()));
                let mut inner = self.desugar_arg_pattern(&pat, &var(&name))?;
                parts.append(&mut inner);
                Ok(parts)
            }
            Pattern::Custom(name, fields) => {
                let mut parts = Vec::new();
                let loc = self.non_shadowed_var(&expr).name();
                parts.push(PatternParts::Bind(
                    loc.clone(),
                    Expression::App(pattern.id, vec![name_expr(name), expr.clone()]),
                ));
                let failed = app(&vec![
                    var(&Symbol::from("_prim_eq")),
                    var(&loc),
                    literal(&Literal::Bool(false)),
                ]);
                let success = app(&vec![var(&Symbol::from("_prim_bitnot")), failed]);
                parts.push(PatternParts::Check(success));
                let size = app(&vec![var(&Symbol::from("_prim_array_size")), var(&loc)]);
                let size_check = app(&vec![
                    var(&Symbol::from("_prim_eq")),
                    size,
                    literal(&Literal::Integer(fields.len() as i64)),
                ]);
                parts.push(PatternParts::Check(size_check));
                for (i, f) in fields.iter().enumerate() {
                    let refer = app(&vec![
                        var(&Symbol::from("_prim_array_ref")),
                        var(&loc),
                        literal(&Literal::Integer(i as i64)),
                    ]);
                    let mut field = self.desugar_arg_pattern(&f, &refer)?;
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
                    Ok(vec![PatternParts::Bind(s.clone(), expr.clone())])
                } else {
                    Ok(vec![])
                }
            }
            Pattern::Literal(lit) => {
                // TODO: Generate appropriate check for each literal type
                let check = app(&vec![
                    var(&Symbol::from("_prim_eq")),
                    expr.clone(),
                    literal(lit),
                ]);
                Ok(vec![PatternParts::Check(check)])
            }
            Pattern::Typed(pat, typ) => {
                let mut inner = self.desugar_arg_pattern(&pat, expr)?;
                let mut val = expr.clone();
                if let Some(PatternParts::Bind(name, exp)) = inner.last() {
                    if exp == expr {
                        val = var(name);
                    }
                }
                let get_type = app(&vec![var(&Symbol::from("_prim_type")), val]);
                let check = app(&vec![
                    var(&Symbol::from("_prim_eq")),
                    get_type,
                    name_expr(typ),
                ]);
                inner.push(PatternParts::Check(check));
                Ok(inner)
            }
            Pattern::Var(Name::Plain(name)) => {
                Ok(vec![PatternParts::Bind(name.clone(), expr.clone())])
            }
            Pattern::Var(_) => self.error("Qualified name in arg pattern"),
            Pattern::Wildcard => Ok(vec![]),
        }
    }

    fn desugar_expression(&mut self, expression: &ExpressionNode) -> Result<Expression> {
        match &expression.expr {
            ast::Expression::App(x) => {
                let mut args = Vec::new();
                for a in x {
                    args.push(self.desugar_expression(&a)?);
                }
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Array(v) => {
                let mut args = Vec::new();
                args.push(self.named_var("_prim_array_make"));
                for a in v {
                    args.push(self.desugar_expression(&a)?);
                }
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Binop(ast::Binop { op, lhs, rhs }) => {
                let mut args = Vec::new();
                args.push(self.var(op.to_name()));
                args.push(self.desugar_expression(&lhs)?);
                args.push(self.desugar_expression(&rhs)?);
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
            ast::Expression::Lambda(funs) => self.desugar_funclauses(expression.id, funs),
            ast::Expression::Literal(lit) => Ok(Expression::Literal(expression.id, lit.clone())),
            ast::Expression::Projection(projs) => {
                let mut args = Vec::new();
                args.push(self.named_var("_prim_project"));
                for a in projs {
                    args.push(self.desugar_expression(&a)?);
                }
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Var(Name::Plain(n)) => Ok(var(&n)),
            ast::Expression::Where(exp, ast_bindings) => {
                let lhs = self.desugar_expression(&exp)?;
                let mut binds = Vec::new();
                for binding in ast_bindings {
                    let mut b = self.desugar_binding(binding, true)?;
                    binds.append(&mut b);
                }
                Ok(Expression::Where(expression.id, Box::new(lhs), binds))
            }
            _ => self.error(&format!("couldn't desugar {}", expression)),
        }
    }

    fn no_match_expression() -> Expression {
        app(&vec![
            var(&Symbol::from("_prim_panic")),
            literal(&Literal::String("No matching pattern".to_owned())),
        ])
    }

    fn desugar_funclauses(&mut self, id: Node, clauses: &[ast::FunClause]) -> Result<Expression> {
        let arity = verify_arity(clauses)?;
        let mut exp = None;
        let args = self.args(&arity);
        for (i, c) in clauses.into_iter().enumerate() {
            let fail_exp = if i + 1 < clauses.len() {
                app(&vec![var(&local(i + 1))])
            } else {
                Self::no_match_expression()
            };
            let (is_irrefutable, next_exp) = self.desugar_funclause(c, &args, &fail_exp)?;
            if let Some(e) = &mut exp {
                let binding = Binding::binding(
                    0,
                    Binder::Local(Name::Plain(local(i))),
                    lambda(vec![], &Arity::Fixed(0), &next_exp),
                );
                match e {
                    Expression::Where(_, exp, bindings) => {
                        bindings.insert(0, binding);
                    }
                    _ => {
                        exp = Some(Expression::Where(0, Box::new(e.clone()), vec![binding]));
                    }
                }
            } else {
                exp = Some(next_exp)
            }
            if is_irrefutable {
                break;
            }
        }
        Ok(lambda(args, &arity, &exp.unwrap()))
    }

    fn desugar_funclause(
        &mut self,
        clause: &ast::FunClause,
        args: &[Var],
        on_fail: &Expression,
    ) -> Result<(bool, Expression)> {
        let body = self.desugar_expression(&clause.body)?;
        let mut all_parts = Vec::new();
        for (p, v) in clause.args.iter().zip(args.iter()) {
            let arg_exp = Expression::Var(0, v.clone());
            let mut parts = self.desugar_arg_pattern(p, &arg_exp)?;
            all_parts.append(&mut parts);
        }
        if let Some(guard) = &clause.guard {
            let guard_exp = self.desugar_expression(&guard)?;
            all_parts.push(PatternParts::Check(guard_exp));
        }
        let is_irrefutable = all_parts
            .iter()
            .all(|p| !matches!(p, PatternParts::Check(_)));
        let mut exp = self.desugar_expression(&clause.body)?;
        for p in all_parts.iter().rev() {
            match p {
                PatternParts::Check(pred) => {
                    exp = cond(pred, &exp, &on_fail);
                }
                PatternParts::Bind(var, expression) => {
                    if let Expression::Var(n, v) = expression {
                        // Just a = b, can rename all 'a' to 'b' instead of
                        // creating a binding.
                        exp = rename(&exp, &Var::Named(var.clone()), v);
                    } else {
                        let binding =
                            Binding::binding(0, Binder::Local(Name::sym(var)), expression.clone());
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
        &mut self,
        pattern: &PatternNode,
        counter: &mut i32,
        replacements: &mut HashMap<Name, Name>,
    ) -> PatternNode {
        let mut p = pattern.clone();
        match &pattern.pattern {
            Pattern::Ellipsis(Some(old)) => {
                let n = Name::Plain(Symbol::from(format!("z{}", counter)));
                *counter += 1;
                replacements.insert(n.clone(), old.clone());
                p.pattern = Pattern::Ellipsis(Some(n));
            }
            Pattern::Var(old) => {
                let n = Name::Plain(Symbol::from(format!("z{}", counter)));
                *counter += 1;
                replacements.insert(n.clone(), old.clone());
                p.pattern = Pattern::Var(n)
            }
            Pattern::Array(arr) => {
                let mut new_arr = Vec::new();
                for p in arr.iter() {
                    new_arr.push(self.pattern_with_plain_vars(p, counter, replacements));
                }
                p.pattern = Pattern::Array(new_arr);
            }
            Pattern::Alias(pat, old) => {
                let n = Name::Plain(Symbol::from(format!("z{}", counter)));
                *counter += 1;
                replacements.insert(n.clone(), old.clone());
                let new_pat = self.pattern_with_plain_vars(pat, counter, replacements);
                p.pattern = Pattern::Alias(Box::new(new_pat), n);
            }
            Pattern::Custom(matcher, fields) => {
                let mut new_fields = Vec::new();
                for p in fields.iter() {
                    new_fields.push(self.pattern_with_plain_vars(p, counter, replacements));
                }
                p.pattern = Pattern::Custom(matcher.clone(), new_fields);
            }
            Pattern::Typed(pat, t) => {
                let new_pat = self.pattern_with_plain_vars(pat, counter, replacements);
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

    fn next_arg(&mut self) -> Var {
        self.last_arg += 1;
        Var::Arg(self.last_arg)
    }

    fn non_shadowed_var(&mut self, exp: &Expression) -> Var {
        let mut vars = HashSet::new();
        let mut locals = HashMap::new();
        free_vars(exp, &mut vars, &mut locals);
        let mut counter = 0;
        loop {
            let next = Symbol::from(format!("x{}", counter));
            if !vars.contains(&next) {
                return Var::Named(next);
            }
            counter += 1;
        }
    }

    fn args(&mut self, arity: &Arity) -> Vec<Var> {
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
        Expression::Var(self.new_id(), Var::Named(Symbol::from(name)))
    }

    fn var(&mut self, name: Name) -> Expression {
        Expression::Var(self.new_id(), Var::Named(name.top()))
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
        Arity::VarArg(len - 1, index as u32)
    } else {
        Arity::Fixed(len)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub metadata: Metadata,
    pub bindings: Vec<Binding>,
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
    pub name: Option<Name>,
    pub body: Expression,
}

impl Binding {
    pub fn binding(id: Node, binder: Binder, body: Expression) -> Self {
        let name = match &binder {
            Binder::Public(name) | Binder::Local(name) => Some(name.clone()),
            Binder::Unbound => None,
        };
        Binding {
            id,
            binder,
            name,
            body,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binder {
    Public(Name),
    Local(Name),
    Unbound,
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
pub struct Let {
    pub var: Var,
    pub expr: Expression,
    pub body: Expression,
}

impl Let {
    pub fn new(var: Var, expr: Expression, body: Expression) -> Self {
        Let { var, expr, body }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub args: Vec<Var>,
    pub arity: Arity,
    pub body: Expression,
}

impl Lambda {
    pub fn new(args: Vec<Var>, arity: Arity, body: Expression) -> Self {
        Lambda { args, arity, body }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    Named(Symbol),
    Arg(u32),
    Env(u32),
    Local(u32),
}

impl Var {
    pub fn name(&self) -> Symbol {
        match self {
            Var::Named(s) => s.clone(),
            Var::Arg(n) => Symbol::from(&format!("$arg{}", n)),
            Var::Env(n) => Symbol::from(format!("$env{}", n)),
            Var::Local(n) => Symbol::from(format!("$loc{}", n)),
        }
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Node, Literal),
    Var(Node, Var),
    Lambda(Node, Box<Lambda>),
    Cond(Node, Box<Cond>),
    App(Node, Vec<Expression>),
    Where(Node, Box<Expression>, Vec<Binding>),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::App(_, exprs) => {
                write!(f, "(")?;
                for e in exprs {
                    write!(f, "{} ", e)?;
                }
                write!(f, ")")?;
            }
            Expression::Cond(_, cond) => {
                write!(f, "({} => {} ; {})", cond.pred, cond.if_true, cond.if_false)?;
            }
            Expression::Lambda(_, lam) => {
                write!(f, "{{ ")?;
                let ellipsis = match lam.arity {
                    Arity::Fixed(_) => None,
                    Arity::VarArg(_, n) => Some(n as usize)
                };
                for (i, a) in lam.args.iter().enumerate() {
                    if let Some(n) = ellipsis && n == i {
                        write!(f, "...")?;
                    }
                    write!(f, "{} ", a.name())?;
                }
                write!(f, "-> {}", lam.body)?;
                write!(f, " }}")?;
            }
            Expression::Literal(_, lit) => {
                write!(f, "{}", lit)?;
            }
            Expression::Var(_, v) => {
                write!(f, "{}", v)?;
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
    Expression::Var(0, Var::Named(s.clone()))
}

fn name_expr(n: &Name) -> Expression {
    match n {
        Name::Plain(s) => var(s),
        Name::Qualified(p, s) => {
            let t = n.top();
            let rest = p[1..].iter().chain(vec![s]);
            let mut args = vec![var(&Symbol::from("project")), var(&t)];
            for a in rest {
                args.push(literal(&Literal::Symbol(a.clone())));
            }
            app(&args)
        }
    }
}

fn arg(n: u32) -> Expression {
    Expression::Var(0, Var::Arg(n))
}

fn env(n: u32) -> Expression {
    Expression::Var(0, Var::Env(n))
}

fn local(n: usize) -> Symbol {
    Symbol::from(format!("$loc{}", n))
}

fn app(exps: &[Expression]) -> Expression {
    Expression::App(0, exps.to_vec())
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

fn lambda(args: Vec<Var>, arity: &Arity, body: &Expression) -> Expression {
    Expression::Lambda(0, Box::new(Lambda::new(args, arity.clone(), body.clone())))
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

pub fn rename(expr: &Expression, old_name: &Var, new_name: &Var) -> Expression {
    match expr {
        Expression::Var(n, v) if v == old_name => Expression::Var(*n, new_name.clone()),
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
        Expression::Lambda(n, lam) => {
            let new_lambda = Lambda::new(
                lam.args.clone(),
                lam.arity.clone(),
                rename(&lam.body, old_name, new_name),
            );
            Expression::Lambda(*n, Box::new(new_lambda))
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
                if let Var::Named(n) = old_name {
                    if let Some(Name::Plain(x)) = &b.name
                        && n == x
                    {
                        shadowed = true;
                    }
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

pub fn free_vars_iter<'a, T>(iter: &'a mut T) -> Result<HashSet<Symbol>>
where
    T: Iterator<Item = &'a Expression>,
{
    let mut vars = HashSet::new();
    let mut locals = HashMap::new();
    for exp in iter {
        free_vars(exp, &mut vars, &mut locals);
    }
    Ok(vars)
}

fn add_local(locals: &mut HashMap<Symbol, i32>, var: &Var) {
    locals.insert(var.name(), locals.get(&var.name()).unwrap_or(&0) + 1);
}

fn remove_local(locals: &mut HashMap<Symbol, i32>, var: &Var) {
    if let Some(v) = locals.get_mut(&var.name()) {
        *v -= 1;
        if *v == 0 {
            locals.remove(&var.name());
        }
    }
}

fn add_locals(locals: &mut HashMap<Symbol, i32>, vars: &[Var]) {
    for v in vars {
        add_local(locals, v);
    }
}

fn remove_locals(locals: &mut HashMap<Symbol, i32>, vars: &[Var]) {
    for v in vars {
        remove_local(locals, v);
    }
}

pub fn free_vars(expr: &Expression, vars: &mut HashSet<Symbol>, locals: &mut HashMap<Symbol, i32>) {
    match expr {
        Expression::Literal(_, literal) => {}
        Expression::Var(_, v) => {
            if locals.get(&v.name()).is_none() {
                vars.insert(v.name());
            }
        }
        Expression::Cond(_, cond) => {
            free_vars(&cond.pred, vars, locals);
            free_vars(&cond.if_false, vars, locals);
            free_vars(&cond.if_true, vars, locals);
        }
        Expression::Lambda(_, lam) => {
            add_locals(locals, &lam.args);
            free_vars(&lam.body, vars, locals);
            remove_locals(locals, &lam.args);
        }
        Expression::App(_, expressions) => {
            for e in expressions {
                free_vars(e, vars, locals);
            }
        }
        Expression::Where(_, expression, bindings) => {
            let mut used = HashSet::new();
            let mut bound = Vec::new();
            for b in bindings {
                match &b.binder {
                    Binder::Local(n) | Binder::Public(n) => {
                        let var = Var::Named(n.top());
                        bound.push(var.clone());
                        add_local(locals, &var);
                    }
                    _ => {}
                };
                free_vars(&b.body, &mut used, locals);
            }
            free_vars(expression, &mut used, locals);
            remove_locals(locals, &bound);
            for v in used {
                vars.insert(v.to_owned());
            }
        }
    }
}
