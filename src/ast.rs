use crate::common::{ Error, Location };
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Metadata {
    pub file: Option<String>,
    pub trivia: Vec<Annotation>,
    pub annotations: HashMap<Node, Annotation>,
    pub locations: HashMap<Node, Location>,
    pub newlines: Vec<usize>,
}

impl Metadata {
    pub fn new(file: Option<String>) -> Self {
         Metadata { file: file, 
            trivia: Vec::new(),
            annotations: HashMap::new(),
            locations: HashMap::new(),
            newlines: Vec::new() }
    }
}

pub type Node = u32;

#[derive(Debug, Clone, PartialEq)]
pub enum Annotation {
    OtherPragma(String),
    Doc(String),
    Comment(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    Module(Module),
    Use(Use),
    VarBinding(VarBinding),
    FunBinding(FunBinding),
    OpBinding(OpBinding)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    id: Node,
    name: Name
}

#[derive(Debug, Clone, PartialEq)]
pub struct Use {
    id: Node,
    name: Name
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarBinding {
    id: Node,
    lhs: Pattern,
    rhs: Expression
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunBinding {
    id: Node,
    name: Name,
    clauses: Vec<FunClause>
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunClause {
    id: Node,
    args: Vec<Pattern>,
    rhs: Expression
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpBinding {
    id: Node,
    op: Operator,
    clauses: Vec<FunClause>
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpClauses {
    id: Node,
    lpat: Pattern,
    rpat: Pattern,
    rhs: Expression
}


// TODO: should guards go into patterns or function argument lists? Should there be and and/or or patterns?
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Ellipsis,
    Literal(Literal),
    Var(Name),
    Array(Vec<Pattern>),
    Alias(Box<Pattern>, String),
    Custom(String, Vec<Pattern>)
}


#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    id: Node,
    expr: ExpressionKind
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Literal(Literal),
    Var(Name),
    Array(Vec<Expression>),
    Lambda(Vec<FunClause>),
    App(Vec<Expression>),
    Binop(Binop),
    Where(Box<Expression>, Vec<Binding>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binop {
    op: Operator,
    lhs: Box<Expression>,
    rhs: Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Nil,
    Bool(bool),
    Integer(u64),
    Real(f64),
    String(String),
    Symbol(String),
    Bytearray(Vec<u8>)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Name {
    Qualified(Vec<String>, String),
    Plain(String)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    Qualified(Vec<String>, String),
    Plain(String)
}

