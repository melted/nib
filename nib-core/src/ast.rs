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

// Declarations
#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Module(ModuleDirective),
    Use(UseDirective),
    Binding(Binding),
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

impl FunClause {
    fn free_vars_helper(&self, vars: &mut HashSet<Name>, locals: &mut HashSet<Name>) {
        let mut exp_local = locals.clone();
        for p in &self.args {
            p.bound_vars_helper(&mut exp_local);
        }
        if let Some(g) = &self.guard {
            g.expr.free_vars_helper(vars, &mut exp_local);
        }
        self.body.expr.free_vars_helper(vars, &mut exp_local);
    }
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

impl OpClause {
    fn free_vars_helper(&self, vars: &mut HashSet<Name>, locals: &mut HashSet<Name>) {
        let mut exp_local = locals.clone();
        self.lpat.bound_vars_helper(&mut exp_local);
        self.rpat.bound_vars_helper(&mut exp_local);
        if let Some(g) = &self.guard {
            g.expr.free_vars_helper(vars, &mut exp_local);
        }
        self.body.expr.free_vars_helper(vars, &mut exp_local);
    }
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
    pub fn is_ellipsis(&self) -> bool {
        matches!(self.pattern, Pattern::Ellipsis(_))
    }

    pub fn bound_vars(&self) -> HashSet<Name> {
        let mut vars = HashSet::new();
        self.bound_vars_helper(&mut vars);
        vars
    }

    fn bound_vars_helper(&self, vars: &mut HashSet<Name>) {
        match &self.pattern {
            Pattern::Ellipsis(name) => {
                if let Some(n) = name {
                    vars.insert(n.clone());
                }
            }
            Pattern::Var(n) => {
                vars.insert(n.clone());
            }
            Pattern::Array(elems) => {
                for e in elems {
                    e.bound_vars_helper(vars);
                }
            }
            Pattern::Alias(p, n) => {
                p.bound_vars_helper(vars);
                vars.insert(n.clone());
            }
            Pattern::Custom(_, fields) => {
                for e in fields {
                    e.bound_vars_helper(vars);
                }
            }
            Pattern::Typed(p, _) => {
                p.bound_vars_helper(vars);
            }
            _ => {}
        }
    }
}

impl Display for PatternNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pattern)
    }
}

impl From<Pattern> for PatternNode {
    fn from(value: Pattern) -> Self {
        PatternNode {
            id: 0,
            pattern: value,
        }
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

impl From<Expression> for ExpressionNode {
    fn from(value: Expression) -> Self {
        ExpressionNode { id: 0, expr: value }
    }
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

impl Expression {
    pub fn free_vars(&self) -> HashSet<Name> {
        let mut vars = HashSet::new();
        let mut locals = HashSet::new();
        self.free_vars_helper(&mut vars, &mut locals);
        vars
    }

    fn free_vars_helper(&self, vars: &mut HashSet<Name>, locals: &mut HashSet<Name>) {
        match self {
            Expression::Var(n) => {
                if !locals.contains(n) {
                    vars.insert(n.clone());
                }
            }
            Expression::Array(elems) => {
                for e in elems {
                    e.expr.free_vars_helper(vars, locals);
                }
            }
            Expression::Lambda(clauses) => {
                for c in clauses {
                    c.free_vars_helper(vars, locals);
                }
            }
            Expression::App(exps) => {
                for e in exps {
                    e.expr.free_vars_helper(vars, locals);
                }
            }
            Expression::Binop(op) => {
                op.lhs.expr.free_vars_helper(vars, locals);
                op.rhs.expr.free_vars_helper(vars, locals);
                let n = op.op.to_name();
                if !locals.contains(&n) {
                    vars.insert(n);
                }
            }
            Expression::Where(exp, binds) => {
                let mut exp_locals = locals.clone();
                for b in binds {
                    match b {
                        Binding::VarBinding(vb) => {
                            vb.rhs.expr.free_vars_helper(vars, &mut exp_locals);
                            let bound = vb.lhs.bound_vars();
                            for n in bound {
                                exp_locals.insert(n);
                            }
                        }
                        Binding::FunBinding(fb) => {
                            let n = fb.name.clone();
                            for c in &fb.clauses {
                                c.free_vars_helper(vars, &mut exp_locals);
                            }
                            exp_locals.insert(n);
                        }
                        Binding::OpBinding(op) => {
                            for c in &op.clauses {
                                c.free_vars_helper(vars, locals);
                            }
                            exp_locals.insert(op.op.to_name());
                        }
                    }
                }
            }
            Expression::Cond(cond) => {
                cond.pred.expr.free_vars_helper(vars, locals);
                cond.on_true.expr.free_vars_helper(vars, locals);
                cond.on_false.expr.free_vars_helper(vars, locals);
            }
            Expression::Projection(proj) => {
                for e in proj {
                    e.expr.free_vars_helper(vars, locals);
                }
            }
            _ => {}
        }
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
