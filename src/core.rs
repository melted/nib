
use std::{collections::HashSet, fmt::{write, Display}};

use crate::{ast::{self, PatternNode}, common::{Metadata, Name, Node, Result}};


pub fn desugar(module: ast::Module) -> Result<Module> {
    let mut state = DesugarState::new(module.metadata);
    state.module_name = state.metadata.base_name.clone();
    for d in module.declarations {
        match d {
            ast::Declaration::Use(ud) => {
                state.metadata.using.insert(ud.name);
            },
            ast::Declaration::Module(md) => {
                state.module_name = Some(md.name);
            },
            ast::Declaration::Binding(bind) => {
                let mut b = state.desugar_binding(bind)?;
                state.bindings.append(&mut b);
            }
        }
    }
    Ok(Module { metadata: state.metadata, bindings: state.bindings })
}


struct DesugarState {
    module_name: Option<Name>,
    bindings: Vec<Binding>,
    metadata: Metadata,
    last_local: u32
}

impl DesugarState {
    fn new(metadata: Metadata) -> Self {
        DesugarState {
            module_name: None,
            bindings: Vec::new(),
            metadata,
            last_local: 0
        }
    }

    fn named(metadata: Metadata, module_name: &Name) -> Self {
        let mut ds = DesugarState::new(metadata);
        ds.module_name = Some(module_name.to_owned());
        ds
    }
}

impl DesugarState {
    fn desugar_binding(&mut self, binding : ast::Binding) -> Result<Vec<Binding>> {
        match binding {
            ast::Binding::FunBinding(fb) => self.desugar_funbinding(fb),
            ast::Binding::OpBinding(ob) => self.desugar_opbinding(ob),
            ast::Binding::VarBinding(vb) => self.desugar_varbinding(vb),
        }
    }

    fn desugar_funbinding(&mut self, ast_binding : ast::FunBinding) -> Result<Vec<Binding>> {
        let name = ast_binding.name;
        let mut core_clauses = Vec::new();
        for clause in ast_binding.clauses {
            let exp = self.desugar_expression(clause.body)?;
            let guard = clause.guard.map(|g| self.desugar_expression(g)).transpose()?;
            core_clauses.push(FunClause { id: clause.id, args: clause.args, guard, rhs: Box::new(exp )});
        }
        self.metadata.last_id += 1;
        Ok(vec![Binding{ id: ast_binding.id, binder: Binder::Public(name), body: Expression::Lambda(self.metadata.last_id, core_clauses)}])
    }


    fn desugar_opbinding(&mut self, ast_binding : ast::OpBinding) -> Result<Vec<Binding>> {
        let name = ast_binding.op.to_name();
        let mut core_clauses = Vec::new();
        for clause in ast_binding.clauses {
            let exp = self.desugar_expression(clause.body)?;
            let guard = clause.guard.map(|g| self.desugar_expression(g)).transpose()?;
            core_clauses.push(FunClause { id: clause.id, args: vec![clause.lpat, clause.rpat], guard, rhs: Box::new(exp )});
        }
        self.metadata.last_id += 1;
        Ok(vec![Binding{ id: ast_binding.id, binder: Binder::Public(name), body: Expression::Lambda(self.metadata.last_id, core_clauses)}])
    }


    fn desugar_varbinding(&mut self, ast_binding : ast::VarBinding) -> Result<Vec<Binding>> {
        let pat = ast_binding.lhs;
        let rhs = self.desugar_expression(ast_binding.rhs)?;
        match pat.pattern {
            ast::Pattern::Var(v) => {
                Ok(vec![Binding { id: ast_binding.id, binder: Binder::Public(v), body: rhs }])
            },
            ast::Pattern::Wildcard => {
                Ok(vec![Binding { id: ast_binding.id, binder: Binder::Unbound, body: rhs }])
            },
            _ => {
                let mut visitor = UsedVars::new();
                pat.visit(&mut visitor);
                let names : Vec<_>= visitor.vars.into_iter().collect();
                let mut v = Vec::new();
                for n in names.clone() {
                    v.push(Expression::Var(self.new_id(), n));
                };
                let mut arr_mk = vec![Expression::Var(self.new_id(), Name::name("array_mk"))];
                arr_mk.append(&mut v);
                let lam_rhs = Expression::App(self.new_id(), arr_mk);
                let lam = Expression::Lambda(self.new_id(), vec![FunClause { id: self.new_id(), args: vec![pat], guard: None, rhs: Box::new(lam_rhs)}]);
                let body = Expression::App(self.new_id(), vec![lam, rhs]);
                let nam_arr = self.next_local();
                let binding = Binding { id: ast_binding.id, binder: Binder::Local(nam_arr.clone()), body };
                let mut bindings = vec![binding];
                for (i, n) in names.into_iter().enumerate() {
                    let rhs = Expression::App(self.new_id(), vec![Expression::Var(self.new_id(), Name::name("array_ref")), Expression::Var(self.new_id(), Name::name(&nam_arr)), Expression::Literal(self.new_id(), ast::Literal::Integer(i as i64))]);
                    let bind = Binding { id: self.new_id(), binder: Binder::Public(n), body: rhs };
                    bindings.push(bind);
                }
                Ok(bindings)

            }  
        }
    }

    fn desugar_pattern(&mut self, pattern : ast::PatternNode) -> Result<Pattern> {
        todo!()
    }

    fn desugar_expression(&mut self, expression : ast::ExpressionNode) -> Result<Expression> {
        match expression.expr {
            ast::Expression::App(x) => {
                let mut args = Vec::new();
                for a in x {
                    args.push(self.desugar_expression(a)?);
                }
                Ok(Expression::App(expression.id, args))
            },
            ast::Expression::Array(v) => {
                let mut args = Vec::new();
                args.push(Expression::Var(self.new_id(), Name::name("array")));
                for a in v {
                    args.push(self.desugar_expression(a)?);
                }
                Ok(Expression::App(expression.id, args))
            },
            ast::Expression::Binop(ast::Binop { op, lhs, rhs }) => {
                let mut args = Vec::new();
                args.push(Expression::Var(self.new_id(), op.to_name()));
                args.push(self.desugar_expression(*lhs)?);
                args.push(self.desugar_expression(*rhs)?);
                Ok(Expression::App(expression.id, args))
            },
            ast::Expression::Cond(cond) => {
                let mut clauses = Vec::new();
                let mut next = cond;
                loop {
                    let guard = self.desugar_expression(*next.pred)?;
                    let rhs = Box::new(self.desugar_expression(*next.on_true)?);
                    let clause = FunClause { id: self.new_id(), args: vec![], guard: Some(guard), rhs };
                    clauses.push(clause);
                    if let ast::Expression::Cond(c) = next.on_false.expr {
                            next = c;
                    } else {
                        let exp = self.desugar_expression(*next.on_false)?;
                        let last = FunClause { id: self.new_id(), args: vec![self.pattern_wildcard()], guard: None, rhs: Box::new(exp) };
                        clauses.push(last);
                        break;
                    }
                }
                Ok(Expression::App(expression.id, vec![Expression::Lambda(self.new_id(), clauses), Expression::Literal(self.new_id(), ast::Literal::Nil)]))
            },
            ast::Expression::Lambda(funs) => {
                let mut clauses = Vec::new(); 
                for f in funs {
                    clauses.push(self.desugar_funclause(f)?);
                }
                Ok(Expression::Lambda(expression.id, clauses))
            },
            ast::Expression::Literal(lit) => Ok(Expression::Literal(expression.id, lit)),
            ast::Expression::Projection(projs) => {
                let mut args = Vec::new();
                args.push(Expression::Var(self.new_id(), Name::name("projection")));
                for a in projs {
                    args.push(self.desugar_expression(a)?);
                }
                Ok(Expression::App(expression.id, args))
            },
            ast::Expression::Var(v) => Ok(Expression::Var(expression.id, v)),
            ast::Expression::Where(exp, ast_bindings) => {
                let lhs = self.desugar_expression(*exp)?;
                let mut binds = Vec::new();
                for binding in ast_bindings {
                    let mut b = self.desugar_binding(binding)?;
                    binds.append(&mut b);
                }
                Ok(Expression::Where(expression.id, Box::new(lhs), binds))
            }
        }
    }

    fn desugar_funclause(&mut self, clause: ast::FunClause) -> Result<FunClause> {
        let exp = self.desugar_expression(clause.body)?;
        let guard = clause.guard.map(|g| self.desugar_expression(g)).transpose()?;
        Ok(FunClause { id: clause.id, args: clause.args, guard, rhs: Box::new(exp)})
    }

    fn next_local(&mut self) -> String {
        self.last_local += 1;
        format!("local.l{}", self.last_local)
    }

    fn new_id(&mut self) -> u32 {
        self.metadata.last_id += 1;
        self.metadata.last_id
    }

    fn pattern_wildcard(&mut self) -> ast::PatternNode {
        PatternNode { id: self.new_id(), pattern: ast::Pattern::Wildcard }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub metadata : Metadata,
    pub bindings : Vec<Binding>
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
    pub id : Node,
    pub binder : Binder,
    pub body : Expression
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binder {
    Public(Name),
    Local(String),
    Unbound
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
    Var(Node, Name),
    Lambda(Node, Vec<FunClause>),
    App(Node, Vec<Expression>),
    Where(Node, Box<Expression>, Vec<Binding>)
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
            },
            Expression::Lambda(_, clauses) => {
                write!(f, "{{ ")?;
                for c in clauses {
                    write!(f, "{};", c)?;
                }
                write!(f, " }}")?;
            },
            Expression::Literal(_, lit) => {
                write!(f, "{}", lit)?;
            },
            Expression::Var(_, v) => {
                write!(f, "{}", v)?;
            },
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
    pub id : Node,
    pub args : Vec<ast::PatternNode>,
    pub guard : Option<Expression>,
    pub rhs : Box<Expression>
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
pub enum Var {
    Named(Name),
    Local(usize),
    Arg(usize)
}

pub enum Pattern {
    Wildcard,
    Ellipsis(Option<Var>),
    Var(Var),
    Array(Vec<Pattern>),
    Custom(Var, Vec<Pattern>),
    Alias(Box<Pattern>, Var)
}

#[derive(Debug)]
pub(super) struct UsedVars {
    pub vars : HashSet<Name>
}

impl UsedVars {
    pub(super) fn new() -> Self {
        UsedVars { vars: HashSet::new() }
    }
}

impl ast::AstVisitor for UsedVars {
    fn on_pattern(&mut self, pat : &ast::PatternNode) -> bool {
        match &pat.pattern {
            ast::Pattern::Var(name) |
            ast::Pattern::Alias(_, name) |
            ast::Pattern::Ellipsis(Some(name))=> {
                self.vars.insert(name.to_owned());
            },
            _ => {}
        };
        true
    }
}