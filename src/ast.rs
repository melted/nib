use crate::common::Location;
use std::collections::{HashMap, HashSet};

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


// Declarations
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Module(Module),
    Use(Use),
    Binding(Binding)
}

impl Declaration {
    pub fn visit(&self, visitor:&mut dyn AstVisitor) {
        if !visitor.on_declaration(self) {
            return;
        }
        match &self {
            Declaration::Binding(b) => match  b {
                Binding::FunBinding(fb) => {
                    for c in &fb.clauses {
                        c.args.iter().map(|p| p.visit(visitor));
                        c.guard.as_ref().map(|g| g.visit(visitor));
                        c.body.visit(visitor);
                    }
                },
                Binding::OpBinding(ob) => {
                    for c in &ob.clauses {
                        c.lpat.visit(visitor);
                        c.rpat.visit(visitor);
                        c.guard.as_ref().map(|e| e.visit(visitor));
                        c.body.visit(visitor);
                    }
                },
                Binding::VarBinding(vb) => {
                    vb.lhs.visit(visitor);
                    vb.rhs.visit(visitor);
                }
            }
            _ => {}
        }
        visitor.on_post_declaration(self);
    }
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
    pub guard: Option<Expression>,
    pub body: Expression
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpBinding {
    pub id: Node,
    pub op: Operator,
    pub clauses: Vec<OpClause>
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpClause {
    pub id: Node,
    pub lpat: Pattern,
    pub rpat: Pattern,
    pub guard: Option<Expression>,
    pub body: Expression
}

// Patterns

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

impl Pattern {
    pub fn visit(&self, visitor: &mut dyn AstVisitor) {
        if !visitor.on_pattern(self) {
            return;
        }
        match &self {
            Pattern::Alias(pat, _) => {
                pat.visit(visitor);
            },
            Pattern::Array(pats) => {
                for p in pats {
                    p.visit(visitor);
                }
            },
            Pattern::Custom(_, pats) => {
                for p in pats {
                    p.visit(visitor);
                }
            },
            _ => {}
        }
        visitor.on_post_pattern(self);
    }
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


impl Expression {
    fn visit(&self, visitor: &mut dyn AstVisitor) {
        if !visitor.on_expression(self) {
            return;
        }
        match &self.expr {
            ExpressionKind::App(f,arg ) => {
                f.visit(visitor);
                arg.visit(visitor);
            },
            ExpressionKind::Array(elems) => {
                for e in elems {
                    e.visit(visitor);
                }
            },
            ExpressionKind::Binop(Binop { op, lhs, rhs }) => {
                lhs.visit(visitor);
                rhs.visit(visitor);
            },
            ExpressionKind::Cond(Cond {pred, on_true, on_false }) => {
                pred.visit(visitor);
                on_true.visit(visitor);
                on_false.visit(visitor);
            },
            ExpressionKind::Lambda(clauses) => {
                for clause in clauses {
                    if let Some(guard) = &clause.guard {
                        guard.visit(visitor);
                    }
                    clause.body.visit(visitor);
                }
            },
            ExpressionKind::Where(exp, bindings) => {

            },
            _ => {}
        }
        visitor.on_post_expression(self);
    }


    pub fn free_variables(&self) -> HashSet<Name> {
        let mut vars = HashSet::new();
  //      let mut bound = HashSet::new();
        vars
    }
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

impl Name {
    pub fn to_string(&self) -> String {
        match self {
            Name::Qualified(path, name) => {
                let mut str = String::new();
                for s in path {
                    str.push_str(s);
                    str.push_str(".");
                }
                str.push_str(name);
                str
            },
            Name::Plain(name) => name.clone()
        }
    }

    pub fn name(n : &str) -> Self {
        let mut parts : Vec<&str> = n.split(".").collect();
        if parts.len() == 1 {
            Name::Plain(parts[0].to_string())
        } else {
            let base = parts.pop().unwrap();
            let path = parts.iter().map(|s| s.to_string()).collect();
            Name::Qualified(path, base.to_string())
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    Qualified(Vec<String>, String),
    Plain(String)
}

pub trait AstVisitor {
    fn on_expression(&mut self, expression: &Expression) -> bool {
        true
    }

    fn on_post_expression(&mut self, expression: &Expression) {

    }

    fn on_declaration(&mut self, decl: &Declaration) -> bool {
        true
    }

    fn on_post_declaration(&mut self, decl: &Declaration) {

    }

    fn on_pattern(&mut self, pat: &Pattern) -> bool {
        true
    }

    fn on_post_pattern(&mut self, pat: &Pattern) {
         
    }
}