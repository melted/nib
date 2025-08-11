use crate::{
    common::{Metadata, Name, Node},
    parser::lexer,
};
use std::{collections::HashSet, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub metadata: Metadata,
    pub declarations: Vec<Declaration>,
}

impl Module {
    pub fn visit(&self, visitor: &mut dyn AstVisitor) {
        for decl in &self.declarations {
            decl.visit(visitor);
        }
    }
}

// Declarations
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Module(ModuleDirective),
    Use(UseDirective),
    Binding(Binding),
}

impl Declaration {
    pub fn visit(&self, visitor: &mut dyn AstVisitor) {
        if !visitor.on_declaration(self) {
            return;
        }
        if let Declaration::Binding(b) = &self { b.visit(visitor) }
        visitor.on_post_declaration(self);
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Module(m) => write!(f, "{}", m),
            Declaration::Use(u) => write!(f, "{}", u),
            Declaration::Binding(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    VarBinding(VarBinding),
    FunBinding(FunBinding),
    OpBinding(OpBinding),
}

impl Binding {
    pub fn visit(&self, visitor: &mut dyn AstVisitor) {
        if !visitor.on_binding(self) {
            return;
        }
        match self {
            Binding::FunBinding(fb) => {
                for c in &fb.clauses {
                    c.args.iter().for_each(|p| p.visit(visitor));
                    if let Some(g) = c.guard.as_ref() { g.visit(visitor) }
                    c.body.visit(visitor);
                }
            }
            Binding::OpBinding(ob) => {
                for c in &ob.clauses {
                    c.lpat.visit(visitor);
                    c.rpat.visit(visitor);
                    if let Some(e) = c.guard.as_ref() { e.visit(visitor) }
                    c.body.visit(visitor);
                }
            }
            Binding::VarBinding(vb) => {
                vb.lhs.visit(visitor);
                vb.rhs.visit(visitor);
            }
        }
        visitor.on_post_binding(self);
    }
}

impl Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Binding::VarBinding(vb) => write!(f, "{}", vb),
            Binding::FunBinding(fb) => write!(f, "{}", fb),
            Binding::OpBinding(ob) => write!(f, "{}", ob),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModuleDirective {
    pub id: Node,
    pub name: Name,
}

impl Display for ModuleDirective {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "module {}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseDirective {
    pub id: Node,
    pub name: Name,
}

impl Display for UseDirective {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "use {}", self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarBinding {
    pub id: Node,
    pub lhs: PatternNode,
    pub rhs: ExpressionNode,
}

impl Display for VarBinding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.lhs, self.rhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunBinding {
    pub id: Node,
    pub name: Name,
    pub clauses: Vec<FunClause>,
}

impl Display for FunBinding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in &self.clauses {
            write!(f, "{} {}; ", self.name, c)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunClause {
    pub id: Node,
    pub args: Vec<PatternNode>,
    pub guard: Option<ExpressionNode>,
    pub body: ExpressionNode,
}

impl Display for FunClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for a in &self.args {
            write!(f, "{} ", a)?;
        }
        if let Some(guard) = &self.guard {
            write!(f, "| {} ", guard)?;
        }
        write!(f, "= {}", self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpBinding {
    pub id: Node,
    pub op: Operator,
    pub clauses: Vec<OpClause>,
}

impl Display for OpBinding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in &self.clauses {
            write!(f, "({}) {}", self.op, c)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpClause {
    pub id: Node,
    pub lpat: PatternNode,
    pub rpat: PatternNode,
    pub guard: Option<ExpressionNode>,
    pub body: ExpressionNode,
}

impl Display for OpClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} ", self.lpat, self.rpat)?;
        if let Some(guard) = &self.guard {
            write!(f, "| {} ", guard)?;
        }
        write!(f, "= {}", self.body)
    }
}

// Patterns
#[derive(Debug, Clone, PartialEq)]
pub struct PatternNode {
    pub id: Node,
    pub pattern: Pattern,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard,
    Ellipsis(Option<Name>),
    Literal(Literal),
    Var(Name),
    Array(Vec<PatternNode>),
    Alias(Box<PatternNode>, Name),
    Custom(Name, Vec<PatternNode>),
    Typed(Box<PatternNode>, Name),
}

impl PatternNode {
    pub fn visit(&self, visitor: &mut dyn AstVisitor) {
        if !visitor.on_pattern(self) {
            return;
        }
        match &self.pattern {
            Pattern::Alias(pat, _) => {
                pat.visit(visitor);
            }
            Pattern::Array(pats) => {
                for p in pats {
                    p.visit(visitor);
                }
            }
            Pattern::Custom(_, pats) => {
                for p in pats {
                    p.visit(visitor);
                }
            }
            Pattern::Typed(pat, _) => {
                pat.visit(visitor);
            }
            _ => {}
        }
        visitor.on_post_pattern(self);
    }
}

impl Display for PatternNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pattern)
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Wildcard => write!(f, "_"),
            Pattern::Alias(pat, alias) => write!(f, "{}@{} ", pat, alias),
            Pattern::Typed(pat, typ) => write!(f, "{}:{} ", pat, typ),
            Pattern::Array(pats) => {
                write!(f, "[")?;
                for (i, p) in pats.iter().enumerate() {
                    write!(f, "{}", p)?;
                    if i < pats.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Pattern::Custom(name, pats) => {
                write!(f, "({}", name)?;
                for p in pats {
                    write!(f, " {}", p)?;
                }
                write!(f, ")")
            }
            Pattern::Ellipsis(name) => write!(
                f,
                "...{}",
                name.clone().map_or(String::new(), |n| n.string())
            ),
            Pattern::Literal(lit) => write!(f, "{}", lit),
            Pattern::Var(var) => write!(f, "{}", var),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionNode {
    pub id: Node,
    pub expr: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Var(Name),
    Array(Vec<ExpressionNode>),
    Lambda(Vec<FunClause>),
    App(Vec<ExpressionNode>),
    Binop(Binop),
    Where(Box<ExpressionNode>, Vec<Binding>),
    Cond(Cond),
    Projection(Vec<ExpressionNode>),
}

impl ExpressionNode {
    pub fn visit(&self, visitor: &mut dyn AstVisitor) {
        if !visitor.on_expression(self) {
            return;
        }
        match &self.expr {
            Expression::App(args) => {
                for arg in args {
                    arg.visit(visitor);
                }
            }
            Expression::Array(elems) => {
                for e in elems {
                    e.visit(visitor);
                }
            }
            Expression::Binop(Binop { op, lhs, rhs }) => {
                lhs.visit(visitor);
                rhs.visit(visitor);
            }
            Expression::Cond(Cond {
                pred,
                on_true,
                on_false,
            }) => {
                pred.visit(visitor);
                on_true.visit(visitor);
                on_false.visit(visitor);
            }
            Expression::Lambda(clauses) => {
                for clause in clauses {
                    if let Some(guard) = &clause.guard {
                        guard.visit(visitor);
                    }
                    clause.body.visit(visitor);
                }
            }
            Expression::Projection(exps) => {
                for e in exps {
                    e.visit(visitor);
                }
            }
            Expression::Where(exp, bindings) => {
                exp.visit(visitor);
                for b in bindings {
                    b.visit(visitor);
                }
            }
            _ => {}
        }
        visitor.on_post_expression(self);
    }

    pub fn free_variables(&self) -> HashSet<Name> {
        
        //      let mut bound = HashSet::new();
        HashSet::new()
    }
}

impl Display for ExpressionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)?;
        Ok(())
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Literal(lit) => write!(f, "{}", lit)?,
            Expression::Var(v) => write!(f, "{}", v)?,
            Expression::App(args) => {
                write!(f, "(")?;
                for arg in args {
                    write!(f, "{} ", arg)?;
                }
                write!(f, ")")?;
            }
            Expression::Array(arr) => {
                write!(f, "[")?;
                for (i, exp) in arr.iter().enumerate() {
                    write!(f, "{}", exp)?;
                    if i < arr.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")?;
            }
            Expression::Binop(Binop { op, lhs, rhs }) => write!(f, "({} {} {})", lhs, op, rhs)?,
            Expression::Cond(Cond {
                pred,
                on_true,
                on_false,
            }) => write!(f, "({} => {} ; {})", pred, on_true, on_false)?,
            Expression::Lambda(clauses) => {
                write!(f, "{{ ")?;
                for c in clauses {
                    for p in &c.args {
                        write!(f, "{p} ")?;
                    }
                    if let Some(guard) = &c.guard {
                        write!(f, "| {} ", guard)?;
                    }
                    write!(f, "-> {}; ", c.body)?;
                }
                write!(f, " }}")?;
            }
            Expression::Projection(exprs) => {
                for (i, exp) in exprs.iter().enumerate() {
                    write!(f, "{}", exp)?;
                    if i < exprs.len() - 1 {
                        write!(f, ".")?;
                    }
                }
            }
            Expression::Where(lhs, bindings) => {
                write!(f, "{} where ", lhs)?;
                for b in bindings {
                    write!(f, "{}; ", b)?;
                }
            }
        };
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binop {
    pub op: Operator,
    pub lhs: Box<ExpressionNode>,
    pub rhs: Box<ExpressionNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cond {
    pub pred: Box<ExpressionNode>,
    pub on_true: Box<ExpressionNode>,
    pub on_false: Box<ExpressionNode>,
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
    Bytearray(Vec<u8>),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Nil => write!(f, "()")?,
            Literal::Bool(b) => write!(f, "{}", b)?,
            Literal::Integer(i) => write!(f, "{}", i)?,
            Literal::Real(r) => write!(f, "{}", r)?,
            Literal::String(s) => write!(f, "\"{}\"", s)?,
            Literal::Char(c) => write!(f, "'{}'", c)?,
            Literal::Symbol(s) => write!(f, "#{}", s)?,
            Literal::Bytearray(ba) => {
                write!(f, "#[")?;
                for (i, b) in ba.iter().enumerate() {
                    write!(f, "{}", b)?;
                    if i < ba.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    Qualified(Vec<String>, String),
    Plain(String),
}

impl Operator {
    pub fn to_name(&self) -> Name {
        match self {
            Operator::Qualified(path, op) => Name::Qualified(path.clone(), operator_id(op)),
            Operator::Plain(op) => Name::Plain(operator_id(op)),
        }
    }
}

fn operator_id(op: &str) -> String {
    if lexer::identifier_initial_char(op.chars().next().unwrap()) {
        op.to_owned()
    } else {
        format!("({})", op)
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Qualified(path, op) => {
                for p in path {
                    write!(f, "{p}.")?;
                }
                write!(f, "{op}")?;
            }
            Operator::Plain(op) => write!(f, "{op}")?,
        }
        Ok(())
    }
}

pub trait AstVisitor {
    fn on_expression(&mut self, expression: &ExpressionNode) -> bool {
        true
    }

    fn on_post_expression(&mut self, expression: &ExpressionNode) {}

    fn on_declaration(&mut self, decl: &Declaration) -> bool {
        true
    }

    fn on_post_declaration(&mut self, decl: &Declaration) {}

    fn on_pattern(&mut self, pat: &PatternNode) -> bool {
        true
    }

    fn on_post_pattern(&mut self, pat: &PatternNode) {}

    fn on_binding(&mut self, binding: &Binding) -> bool {
        true
    }

    fn on_post_binding(&mut self, binding: &Binding) {}
}
