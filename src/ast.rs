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
pub enum Declaration {
    Module(Module),
    Use(Use),
    Binding(Binding)
}


#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    VarBinding(VarBinding),
    FunBinding(FunBinding),
    OpBinding(OpBinding)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub id: Node,
    pub name: Name
}

#[derive(Debug, Clone, PartialEq)]
pub struct Use {
    pub id: Node,
    pub name: Name
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarBinding {
    pub id: Node,
    pub lhs: Pattern,
    pub rhs: Expression
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunBinding {
    pub id: Node,
    pub name: Name,
    pub clauses: Vec<FunClause>
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunClause {
    pub id: Node,
    pub args: Vec<Pattern>,
    pub rhs: Expression
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpBinding {
    pub id: Node,
    pub op: Operator,
    pub clauses: Vec<FunClause>
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpClauses {
    pub id: Node,
    pub lpat: Pattern,
    pub rpat: Pattern,
    pub rhs: Expression
}


// TODO: should guards go into patterns or function argument lists? Should there be and and/or or patterns?
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Ellipsis(Name),
    Literal(Literal),
    Var(Name),
    Array(Vec<Pattern>),
    Alias(Box<Pattern>, Name),
    Custom(Name, Vec<Pattern>)
}


#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub id: Node,
    pub expr: ExpressionKind
}


#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Literal(Literal),
    Var(Name),
    Array(Vec<Expression>),
    Lambda(Vec<FunClause>),
    App(Box<Expression>, Box<Expression>),
    Binop(Binop),
    Where(Box<Expression>, Vec<Binding>),
    Cond(Cond)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binop {
    pub op: Operator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cond {
    pub pred: Box<Expression>,
    pub on_true: Box<Expression>,
    pub on_false: Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Nil,
    Bool(bool),
    Integer(i64),
    Real(f64),
    Char(char),
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

