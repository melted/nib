use std::{collections::HashSet, fmt::Display};

use crate::{
    ast::{self, Literal, PatternNode},
    common::{Error, Metadata, Name, Node, Result},
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

pub fn desugar_expression(expr: ast::ExpressionNode) -> Result<Expression> {
    let meta = Metadata::new(None);
    let mut state = DesugarState::new(meta);
    state.desugar_expression(&expr)
}

struct DesugarState {
    module_name: Option<Name>,
    bindings: Vec<Binding>,
    metadata: Metadata,
    last_local: u32,
}

impl DesugarState {
    fn new(metadata: Metadata) -> Self {
        DesugarState {
            module_name: None,
            bindings: Vec::new(),
            metadata,
            last_local: 0,
        }
    }

    fn named(metadata: Metadata, module_name: &Name) -> Self {
        let mut ds = DesugarState::new(metadata);
        ds.module_name = Some(module_name.to_owned());
        ds
    }
}

impl DesugarState {
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
            Binder::Local(ast_binding.name.string())
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
                Binder::Local(name.string())
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
        let pat = &ast_binding.lhs;
        let rhs = self.desugar_expression(&ast_binding.rhs)?;
        match &pat.pattern {
            ast::Pattern::Var(v) => {
                let binder = if is_local {
                    Binder::Local(v.string())
                } else {
                    Binder::Public(self.desugar_binding_name(&v)?)
                };
                Ok(vec![Binding::binding(ast_binding.id, binder, rhs)])
            }
            ast::Pattern::Wildcard => {
                Ok(vec![Binding::binding(ast_binding.id, Binder::Unbound, rhs)])
            }
            _ => {
                let mut visitor = UsedVars::new();
                pat.visit(&mut visitor);
                let names: Vec<_> = visitor.vars.into_iter().collect();
                let mut v = Vec::new();
                for n in names.clone() {
                    v.push(self.var(n));
                }
                let mut arr_mk = vec![self.named_var("_prim_array_mk")];
                arr_mk.append(&mut v);
                let lam_rhs = Expression::App(self.new_id(), arr_mk);
                let npat: Vec<String> = names.iter().map(|n| n.string()).collect();
                let arity = Arity::Fixed(npat.len() as u32);
                let lam = Expression::Lambda(self.new_id(), Box::new(Lambda::new(arity, lam_rhs)));
                let body = Expression::App(self.new_id(), vec![lam, rhs]);
                let nam_arr = self.next_local();
                let binding =
                    Binding::binding(ast_binding.id, Binder::Local(nam_arr.clone()), body);
                let mut bindings = vec![binding];
                for (i, n) in names.into_iter().enumerate() {
                    let rhs = Expression::App(
                        self.new_id(),
                        vec![
                            self.named_var("_prim_array_ref"),
                            self.named_var(&nam_arr),
                            Expression::Literal(self.new_id(), ast::Literal::Integer(i as i64)),
                        ],
                    );
                    let binder = if is_local {
                        Binder::Local(n.string())
                    } else {
                        Binder::Public(self.desugar_binding_name(&n)?)
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

    fn desugar_pattern(
        &mut self,
        pattern: &ast::PatternNode,
        expr: &Expression,
    ) -> Result<Vec<PatternParts>> {
        match &pattern.pattern {
            ast::Pattern::Alias(pat, alias) => {
                let name = alias.string();
                let mut parts = Vec::new();
                parts.push(PatternParts::Bind(name.clone(), expr.to_owned()));
                let mut inner = self.desugar_pattern(&pat, &Expression::Var(0, name))?;
                parts.append(&mut inner);
                Ok(parts)
            }
            ast::Pattern::Custom(name, fields) => {
                let mut parts = Vec::new();
                let var = self.next_local();
                parts.push(PatternParts::Bind(
                    var.clone(),
                    Expression::App(
                        pattern.id,
                        vec![Expression::Var(0, name.string()), expr.clone()],
                    ),
                ));
                let refer = Expression::Var(0, var.clone());
                for f in fields {
                    let mut field = self.desugar_pattern(&f, &refer)?;
                    parts.append(&mut field);
                }
                Ok(parts)
            }
            ast::Pattern::Array(fields) => self.desugar_pattern(
                &PatternNode {
                    id: 0,
                    pattern: ast::Pattern::Custom(Name::str("array"), fields.to_vec()),
                },
                expr,
            ),
            ast::Pattern::Ellipsis(name) => {
                if let Some(    n) = name {
                    Ok(vec![PatternParts::Bind(n.string(), expr.clone())])
                } else {
                    Ok(vec![])
                }
            }
            ast::Pattern::Literal(lit) => {
                let check = app(&vec![var("_prim_eq"), expr.clone(), literal(lit)]);
                Ok(vec![PatternParts::Check(check)])
            }
            ast::Pattern::Typed(pat, typ) => {
                let mut inner = self.desugar_pattern(&pat, expr)?;
                let mut val = expr.clone();
                if let Some(PatternParts::Bind(name, exp)) = inner.last() {
                    if exp == expr {
                        val = var(name);
                    }
                }
                let get_type = app(&vec![var("_prim_type"), val]);
                let check = app(&vec![var("_prim_eq"), get_type, var(&typ.string())]);
                inner.push(PatternParts::Check(check));
                Ok(inner)
            }
            ast::Pattern::Var(name) => Ok(vec![PatternParts::Bind(name.string(), expr.clone())]),
            ast::Pattern::Wildcard => Ok(vec![]),
        }
    }

    fn desugar_expression(&mut self, expression: &ast::ExpressionNode) -> Result<Expression> {
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
            ast::Expression::Lambda(funs) => {
                self.desugar_funclauses(expression.id, funs)
            }
            ast::Expression::Literal(lit) => Ok(Expression::Literal(expression.id, lit.clone())),
            ast::Expression::Projection(projs) => {
                let mut args = Vec::new();
                args.push(self.named_var("_prim_project"));
                for a in projs {
                    args.push(self.desugar_expression(&a)?);
                }
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Var(Name::Plain(n)) => Ok(Expression::Var(expression.id, n.clone())),
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
        app(&vec![var("_prim_panic"), literal(&Literal::String("No matching pattern".to_owned()))])
    }

    fn desugar_funclauses(&mut self, id: Node, clauses: &[ast::FunClause]) -> Result<Expression> {
        let arity = verify_arity(clauses)?;
        let on_fail = Self::no_match_expression();
        let mut exp = on_fail;
        for c in clauses.into_iter().rev() {
            exp = self.desugar_funclause(c, &exp)?;
        }
        Ok(Expression::Lambda(id, Box::new(Lambda::new(arity, exp))))
    }

    fn desugar_funclause(
        &mut self,
        clause: &ast::FunClause,
        on_fail: &Expression,
    ) -> Result<Expression> {
        let body = self.desugar_expression(&clause.body)?;
        todo!()
    }

    fn next_local(&mut self) -> String {
        self.last_local += 1;
        format!("local.l{}", self.last_local)
    }

    fn new_id(&mut self) -> u32 {
        self.metadata.last_id += 1;
        self.metadata.last_id
    }

    fn named_var(&mut self, name: &str) -> Expression {
        Expression::Var(self.new_id(), name.to_string())
    }

    fn var(&mut self, name: Name) -> Expression {
        Expression::Var(self.new_id(), name.string())
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

pub fn get_arity(patterns: &[ast::PatternNode]) -> Arity {
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
    pub name: String,
    pub body: Expression,
}

impl Binding {
    pub fn binding(id: Node, binder: Binder, body: Expression) -> Self {
        let name = match &binder {
            Binder::Public(name) => name.string(),
            Binder::Local(s) => s.clone(),
            Binder::Unbound => "".to_string(),
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
    Local(String),
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
    pub var: String,
    pub expr: Expression,
    pub body: Expression,
}

impl Let {
    pub fn new(var: String, expr: Expression, body: Expression) -> Self {
        Let { var, expr, body }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub arity: Arity,
    pub body: Expression,
}

impl Lambda {
    pub fn new(arity: Arity, body: Expression) -> Self {
        Lambda { arity, body }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Node, ast::Literal),
    Var(Node, String),
    Arg(u32),
    Lambda(Node, Box<Lambda>),
    Let(Node, Box<Let>),
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
            Expression::Arg(n) => {
                write!(f, "arg{}", n);
            }
            Expression::Cond(_, cond) => {
                write!(f, "({} => {} ; {})", cond.pred, cond.if_true, cond.if_false)?;
            }
            Expression::Let(_, bind) => {
                write!(f, "(let {} = {} in {})", bind.var, bind.expr, bind.body)?;
            }
            Expression::Lambda(_, lam) => {
                write!(f, "{{ ")?;
                write!(f, "{} ", lam.arity)?;
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

fn var(s: &str) -> Expression {
    Expression::Var(0, s.to_owned())
}

fn app(exps: &[Expression]) -> Expression {
    Expression::App(0, exps.to_vec())
}

fn literal(lit: &Literal) -> Expression {
    Expression::Literal(0, lit.clone())
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternParts {
    Check(Expression),
    Bind(String, Expression),
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

pub fn rename(expr: &mut Expression, old_name: &str, new_name: &str) {
    match expr {
        Expression::Var(_, v) if v == old_name => {
            v.clear();
            v.push_str(&new_name);
        }
        Expression::App(_, exps) => {
            for e in exps {
                rename(e, old_name, new_name);
            }
        }
        Expression::Cond(_, cond) => {
            rename(&mut cond.pred, old_name, new_name);
            rename(&mut cond.if_true, old_name, new_name);
            rename(&mut cond.if_false, old_name, new_name);
        }
        Expression::Lambda(_, lam) => {
            rename(&mut lam.body, old_name, new_name);
        }
        Expression::Let(_, bind) => {
            rename(&mut bind.expr, old_name, new_name);
            if bind.var != old_name {
                rename(&mut bind.body, old_name, new_name);
            }
        }
        Expression::Where(_, exp, defs) => {
            let mut shadowed = false;
            for b in defs {
                rename(&mut b.body, old_name, new_name);
                if b.name == old_name {
                    shadowed = true;
                    break;
                }
            }
            if !shadowed {
                rename(&mut *exp, old_name, new_name);
            }
        }
        _ => {}
    }
}

pub fn free_vars_iter<'a, T>(iter : &'a mut T) -> Result<HashSet<String>> where T:Iterator<Item = &'a Expression> {
    let mut vars = HashSet::new();
    for exp in iter {
        free_vars(exp, &mut vars)?;
    }
    Ok(vars)
}

pub fn free_vars(expr: &Expression, vars: &mut HashSet<String>) -> Result<()> {
    match expr {
        Expression::Literal(_, literal) => {}
        Expression::Arg(_) => {}
        Expression::Var(_, var) => {
            vars.insert(var.to_owned());
        }
        Expression::Let(_, clause) => {
            let shadowed = vars.contains(&clause.var);
            free_vars(&clause.expr, vars)?;
            free_vars(&clause.body, vars)?;
            if !shadowed {
                vars.remove(&clause.var);
            }
        }
        Expression::Cond(_, cond) => {
            free_vars(&cond.pred, vars)?;
            free_vars(&cond.if_false, vars)?;
            free_vars(&cond.if_true, vars)?;
        }
        Expression::Lambda(_, lam) => {
            let mut used = HashSet::new();
            let mut bound = HashSet::new();
            free_vars(&lam.body, &mut used)?;

            for v in used.difference(&bound) {
                vars.insert(v.to_owned());
            }
        }
        Expression::App(_, expressions) => {
            for e in expressions {
                free_vars(e, vars)?;
            }
        }
        Expression::Where(_, expression, bindings) => {
            let mut used = HashSet::new();
            let mut bound = HashSet::new();
            for b in bindings {
                free_vars(&b.body, &mut used)?;
                match &b.binder {
                    Binder::Local(n) => {
                        bound.insert(n.to_owned());
                    }
                    Binder::Public(n) => {
                        bound.insert(n.string());
                    }
                    _ => {}
                };
            }
            free_vars(expression, &mut used)?;
            for v in used.difference(&bound) {
                vars.insert(v.to_owned());
            }
        }
    }
    Ok(())
}

#[derive(Debug)]
pub(super) struct UsedVars {
    pub vars: HashSet<Name>,
}

impl UsedVars {
    pub(super) fn new() -> Self {
        UsedVars {
            vars: HashSet::new(),
        }
    }
}

impl ast::AstVisitor for UsedVars {
    fn on_pattern(&mut self, pat: &ast::PatternNode) -> bool {
        match &pat.pattern {
            ast::Pattern::Var(name)
            | ast::Pattern::Alias(_, name)
            | ast::Pattern::Ellipsis(Some(name)) => {
                self.vars.insert(name.to_owned());
            }
            _ => {}
        };
        true
    }
}
