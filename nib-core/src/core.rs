use std::{
    collections::HashSet,
    fmt::Display,
};

use crate::{
    ast::{self},
    common::{Error, Metadata, Name, Node, Result},
};

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
                let mut b = state.desugar_binding(bind)?;
                state.bindings.append(&mut b);
            }
        }
    }
    Ok(Module {
        metadata: state.metadata,
        bindings: state.bindings,
    })
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
    fn desugar_binding(&mut self, binding: ast::Binding) -> Result<Vec<Binding>> {
        match binding {
            ast::Binding::FunBinding(fb) => self.desugar_funbinding(fb),
            ast::Binding::OpBinding(ob) => self.desugar_opbinding(ob),
            ast::Binding::VarBinding(vb) => self.desugar_varbinding(vb),
        }
    }

    fn desugar_funbinding(&mut self, ast_binding: ast::FunBinding) -> Result<Vec<Binding>> {
        let name = ast_binding.name;
        let mut core_clauses = Vec::new();
        for clause in ast_binding.clauses {
            let exp = self.desugar_expression(clause.body)?;
            let guard = clause
                .guard
                .map(|g| self.desugar_expression(g))
                .transpose()?;
            let mut args = Vec::new();
            for a in clause.args {
                args.push(self.desugar_pattern(a)?);
            }
            core_clauses.push(FunClause {
                id: clause.id,
                args,
                guard,
                rhs: Box::new(exp),
            });
        }
        self.metadata.last_id += 1;
        Ok(vec![Binding::binding(
            ast_binding.id,
            Binder::Public(name),
            Expression::Lambda(self.metadata.last_id, core_clauses))
        ])
    }

    fn desugar_opbinding(&mut self, ast_binding: ast::OpBinding) -> Result<Vec<Binding>> {
        let name = ast_binding.op.to_name();
        let mut core_clauses = Vec::new();
        for clause in ast_binding.clauses {
            let exp = self.desugar_expression(clause.body)?;
            let guard = clause
                .guard
                .map(|g| self.desugar_expression(g))
                .transpose()?;
            let lpat = self.desugar_pattern(clause.lpat)?;
            let rpat = self.desugar_pattern(clause.rpat)?;
            core_clauses.push(FunClause {
                id: clause.id,
                args: vec![lpat, rpat],
                guard,
                rhs: Box::new(exp),
            });
        }
        self.metadata.last_id += 1;
        Ok(vec![Binding::binding(
           ast_binding.id,
            Binder::Public(name),
            Expression::Lambda(self.metadata.last_id, core_clauses),
        )])
    }

    fn desugar_varbinding(&mut self, ast_binding: ast::VarBinding) -> Result<Vec<Binding>> {
        let pat = ast_binding.lhs;
        let rhs = self.desugar_expression(ast_binding.rhs)?;
        match pat.pattern {
            ast::Pattern::Var(v) => Ok(vec![Binding::binding(
                ast_binding.id,
                Binder::Public(v),
                rhs,
            )]),
            ast::Pattern::Wildcard => Ok(vec![Binding::binding(
                ast_binding.id,
                Binder::Unbound,
                rhs,)
            ]),
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
                let npat = self.desugar_pattern(pat)?;
                let lam = Expression::Lambda(
                    self.new_id(),
                    vec![FunClause {
                        id: self.new_id(),
                        args: vec![npat],
                        guard: None,
                        rhs: Box::new(lam_rhs),
                    }],
                );
                let body = Expression::App(self.new_id(), vec![lam, rhs]);
                let nam_arr = self.next_local();
                let binding = Binding::binding(                    ast_binding.id,
                    Binder::Local(nam_arr.clone()),
                    body,
                );
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
                    let bind = Binding::binding(
                        self.new_id(),
                        Binder::Public(n),
                        rhs,
                    );
                    bindings.push(bind);
                }
                Ok(bindings)
            }
        }
    }

    fn desugar_pattern(&mut self, pattern: ast::PatternNode) -> Result<Pattern> {
        match pattern.pattern {
            ast::Pattern::Alias(pat, alias) => {
                let inner = self.desugar_pattern(*pat)?;
                Ok(Pattern::Alias(Box::new(inner), alias))
            }
            ast::Pattern::Custom(name, fields) => {
                let mut args = Vec::new();
                for f in fields {
                    args.push(self.desugar_pattern(f)?);
                }
                Ok(Pattern::Custom(name, args))
            }
            ast::Pattern::Array(fields) => {
                let mut args = Vec::new();
                for f in fields {
                    args.push(self.desugar_pattern(f)?);
                }
                Ok(Pattern::Custom(Name::name("array"), args))
            }
            ast::Pattern::Ellipsis(name) => Ok(Pattern::Ellipsis(name)),
            ast::Pattern::Literal(lit) => Ok(Pattern::Literal(lit)),
            ast::Pattern::Typed(pat, typ) => {
                let inner = self.desugar_pattern(*pat)?;
                Ok(Pattern::Custom(typ, vec![inner]))
            }
            ast::Pattern::Var(name) => Ok(Pattern::Bind(name)),
            ast::Pattern::Wildcard => Ok(Pattern::Wildcard),
        }
    }

    fn desugar_expression(&mut self, expression: ast::ExpressionNode) -> Result<Expression> {
        match expression.expr {
            ast::Expression::App(x) => {
                let mut args = Vec::new();
                for a in x {
                    args.push(self.desugar_expression(a)?);
                }
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Array(v) => {
                let mut args = Vec::new();
                args.push(self.named_var("array"));
                for a in v {
                    args.push(self.desugar_expression(a)?);
                }
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Binop(ast::Binop { op, lhs, rhs }) => {
                let mut args = Vec::new();
                args.push(self.var(op.to_name()));
                args.push(self.desugar_expression(*lhs)?);
                args.push(self.desugar_expression(*rhs)?);
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Cond(cond) => {
                let mut clauses = Vec::new();
                let mut next = cond;
                loop {
                    let guard = self.desugar_expression(*next.pred)?;
                    let rhs = Box::new(self.desugar_expression(*next.on_true)?);
                    let clause = FunClause {
                        id: self.new_id(),
                        args: vec![],
                        guard: Some(guard),
                        rhs,
                    };
                    clauses.push(clause);
                    if let ast::Expression::Cond(c) = next.on_false.expr {
                        next = c;
                    } else {
                        let exp = self.desugar_expression(*next.on_false)?;
                        let last = FunClause {
                            id: self.new_id(),
                            args: vec![Pattern::Wildcard],
                            guard: None,
                            rhs: Box::new(exp),
                        };
                        clauses.push(last);
                        break;
                    }
                }
                Ok(Expression::App(
                    expression.id,
                    vec![
                        Expression::Lambda(self.new_id(), clauses),
                        Expression::Literal(self.new_id(), ast::Literal::Nil),
                    ],
                ))
            }
            ast::Expression::Lambda(funs) => {
                let mut clauses = Vec::new();
                for f in funs {
                    clauses.push(self.desugar_funclause(f)?);
                }
                Ok(Expression::Lambda(expression.id, clauses))
            }
            ast::Expression::Literal(lit) => Ok(Expression::Literal(expression.id, lit)),
            ast::Expression::Projection(projs) => {
                let mut args = Vec::new();
                args.push(self.named_var("project"));
                for a in projs {
                    args.push(self.desugar_expression(a)?);
                }
                Ok(Expression::App(expression.id, args))
            }
            ast::Expression::Var(Name::Plain(n)) => Ok(Expression::Var(expression.id, n)),
            ast::Expression::Where(exp, ast_bindings) => {
                let lhs = self.desugar_expression(*exp)?;
                let mut binds = Vec::new();
                for binding in ast_bindings {
                    let mut b = self.desugar_binding(binding)?;
                    binds.append(&mut b);
                }
                Ok(Expression::Where(expression.id, Box::new(lhs), binds))
            }
            _ => self.error(&format!("couldn't desugar {}", expression)),
        }
    }

    fn desugar_funclause(&mut self, clause: ast::FunClause) -> Result<FunClause> {
        let exp = self.desugar_expression(clause.body)?;
        let mut args = Vec::new();
        for a in clause.args {
            args.push(self.desugar_pattern(a)?);
        }
        let guard = clause
            .guard
            .map(|g| self.desugar_expression(g))
            .transpose()?;
        Ok(FunClause {
            id: clause.id,
            args,
            guard,
            rhs: Box::new(exp),
        })
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
        Expression::Var(self.new_id(), name.to_string())
    }

    fn error<T>(&self, msg: &str) -> Result<T> {
        Err(Error::Desugar {
            msg: msg.to_owned(),
            loc: None
        })
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
    pub fn binding(id:Node, binder:Binder, body:Expression) -> Self {
        let name = match &binder {
            Binder::Public(name) => name.to_string(),
            Binder::Local(s) => s.clone(),
            Binder::Unbound => "".to_string(),
        };
        Binding { id, binder, name, body }
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
pub enum Expression {
    Literal(Node, ast::Literal),
    Var(Node, String),
    Lambda(Node, Vec<FunClause>),
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
            Expression::Lambda(_, clauses) => {
                write!(f, "{{ ")?;
                for c in clauses {
                    write!(f, "{};", c)?;
                }
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

#[derive(Debug, Clone, PartialEq)]
pub struct FunClause {
    pub id: Node,
    pub args: Vec<Pattern>,
    pub guard: Option<Expression>,
    pub rhs: Box<Expression>,
}

impl Display for FunClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for a in &self.args {
            write!(f, "{} ", a)?;
        }
        if let Some(guard) = &self.guard {
            write!(f, "| {} ", guard)?;
        }
        write!(f, "-> {}", self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Literal(ast::Literal), // TODO: Create a binding to it instead?
    Ellipsis(Option<Name>),
    Bind(Name),
    Custom(Name, Vec<Pattern>), // Subsumes array and typed patterns
    Alias(Box<Pattern>, Name),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arity {
    Fixed(usize),
    VarArg(usize),
}

impl Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Arity::Fixed(n) => write!(f, "{}", n),
            Arity::VarArg(n) => write!(f, "{}+", n),
        }
    }
}

impl Arity {
    pub fn min_arity(&self) -> usize {
        match self {
            Arity::Fixed(n) | Arity::VarArg(n) => *n
        }
    }
}

impl Pattern {
    pub fn is_ellipsis(&self) -> bool {
        match self {
            Pattern::Ellipsis(_) => true,
            _ => false,
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Alias(pat, alias) => write!(f, "{}@{}", pat, alias),
            Pattern::Bind(name) => write!(f, "{}", name),
            Pattern::Custom(name, fields) => {
                write!(f, "({}", name)?;
                for field in fields {
                    write!(f, " {}", field)?;
                }
                write!(f, ")")
            }
            Pattern::Ellipsis(name) => {
                write!(f, "...")?;
                if let Some(n) = name {
                    write!(f, "{}", n)?;
                }
                Ok(())
            }
            Pattern::Literal(lit) => write!(f, "{}", lit),
            Pattern::Wildcard => write!(f, "_"),
        }
    }
}

pub fn free_vars(expr: &Expression, vars: &mut HashSet<String>) {
    match expr {
        Expression::Literal(_, literal) => {}
        Expression::Var(_, var) => {
            vars.insert(var.to_owned());
        }
        Expression::Lambda(_, fun_clauses) => {
            for c in fun_clauses {
                let mut used = HashSet::new();
                let mut bound = HashSet::new();
                if let Some(g) = &c.guard {
                    free_vars(g, &mut used);
                }
                free_vars(&c.rhs, &mut used);
                for p in &c.args {
                    bound_vars(p, &mut bound);
                }

                for v in used.difference(&bound) {
                    vars.insert(v.to_owned());
                }
            }
        }
        Expression::App(_, expressions) => {
            for e in expressions {
                free_vars(e, vars);
            }
        }
        Expression::Where(_, expression, bindings) => {
            let mut used = HashSet::new();
            let mut bound = HashSet::new();
            for b in bindings {
                free_vars(expr, &mut used);
                match &b.binder {
                    Binder::Local(n) => {
                        bound.insert(n.to_owned());
                    }
                    Binder::Public(n) => {
                        bound.insert(n.to_string());
                    }
                    _ => {}
                };
            }
            free_vars(&expression, &mut used);
            for v in used.difference(&bound) {
                vars.insert(v.to_owned());
            }
        }
    }
}

pub fn bound_vars(pat: &Pattern, vars: &mut HashSet<String>) {
    match pat {
        Pattern::Ellipsis(Some(name)) | Pattern::Bind(name) => {
            vars.insert(name.to_string());
        }
        Pattern::Custom(name, patterns) => {
            for p in patterns {
                bound_vars(p, vars);
            }
        }
        Pattern::Alias(pattern, name) => {
            bound_vars(&pattern, vars);
            vars.insert(name.to_string());
        }
        _ => {}
    }
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
