
use std::collections::HashSet;

use crate::{ast, common::{Metadata, Name, Node, Result}};


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
        Ok(vec![Binding{ id: ast_binding.id, name, body: Expression::Lambda(self.metadata.last_id, core_clauses)}])
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
        Ok(vec![Binding{ id: ast_binding.id, name, body: Expression::Lambda(self.metadata.last_id, core_clauses)}])
    }


    fn desugar_varbinding(&mut self, ast_binding : ast::VarBinding) -> Result<Vec<Binding>> {
        let pat = ast_binding.lhs;
        let rhs = self.desugar_expression(ast_binding.rhs)?;
        match pat.pattern {
            ast::Pattern::Var(v) => {
                Ok(vec![Binding { id: ast_binding.id, name: v, body: rhs }])
            },
            ast::Pattern::Wildcard => {
                Ok(vec![Binding { id: ast_binding.id, name: Name::Plain("".to_string()), body: rhs }])
            },
            _ => {
                let mut visitor = UsedVars::new();
                pat.visit(&mut visitor);
                let names : Vec<_>= visitor.vars.into_iter().collect();
                let v = names.clone().into_iter().map(|n| Expression::Var(self.new_id(), n)).collect();
                let lam_rhs = Expression::App(self.new_id(), Box::new(Expression::Var(self.new_id(), Name::name("array_mk"))), v);
                let lam = Expression::Lambda(self.new_id(), vec![FunClause { id: self.new_id(), args: vec![pat], guard: None, rhs: Box::new(lam_rhs)}]);
                let body = Expression::App(self.new_id(), Box::new(lam), vec![rhs]);
                let binding = Binding { id: ast_binding.id, name: self.next_local(), body };
                let mut bindings = vec![binding];
                for (i, n) in names.into_iter().enumerate() {
                    let rhs = Expression::App(self.new_id(), Box::new(Expression::Var(self.new_id(), Name::name("array_ref"))), vec![Expression::Literal(self.new_id(), ast::Literal::Integer(i as i64))]);
                    let bind = Binding { id: self.new_id(), name: n, body: rhs };
                    bindings.push(bind);
                }
                Ok(bindings)

            }  
        }
    }

    fn desugar_expression(&mut self, expression : ast::ExpressionNode) -> Result<Expression> {
        todo!()
    }

    fn next_local(&mut self) -> Name {
        self.last_local += 1;
        Name::name(&format!("local.l{}", self.last_local))
    }

    fn new_id(&mut self) -> u32 {
        self.metadata.last_id += 1;
        self.metadata.last_id
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub metadata : Metadata,
    pub bindings : Vec<Binding>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub id : Node,
    pub name : Name,
    pub body : Expression
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Node, ast::Literal),
    Var(Node, Name),
    Lambda(Node, Vec<FunClause>),
    App(Node, Box<Expression>, Vec<Expression>),
    Where(Node, Box<Expression>, Vec<Binding>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunClause {
    pub id : Node,
    pub args : Vec<ast::PatternNode>,
    pub guard : Option<Expression>,
    pub rhs : Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    Named(Name),
    Local(u32)
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
            ast::Pattern::Var(name) => {
                self.vars.insert(name.to_owned());
            },
            _ => {}
        };
        true
    }
}