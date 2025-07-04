use crate::{ast, common::{Metadata, Name, Node}};

pub fn desugar(module: ast::Module) -> Module {
    todo!()
}

pub struct Module {
    pub metadata : Metadata,
    pub bindings : Vec<Binding>
}

pub struct Binding {
    pub id : Node,
    pub name : Name,
    pub body : Expression
}

pub enum Expression {
    Literal(Node, ast::Literal),
    Var(Node, Var),
    Lambda(Node, Vec<FunClause>),
    App(Node, Box<Expression>, Vec<Expression>),
    Where(Node, Box<Expression>, Vec<Binding>)
}

pub struct FunClause {
    pub id : Node,
    pub args : Vec<ast::Pattern>,
    pub rhs : Box<Expression>
}

pub enum Var {
    Named(String),
    Local(u32)
}