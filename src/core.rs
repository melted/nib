
use crate::{ast, common::{Metadata, Name, Node, Result}};

pub fn desugar(module: ast::Module) -> Result<Module> {
    let mut metadata = module.metadata;
    let mut module_name = metadata.base_name.clone().unwrap_or(Name::name("default"));
    let mut bindings = Vec::new();
    for d in module.declarations {
        match d {
            ast::Declaration::Use(ud) => {
                metadata.using.insert(ud.name);
            },
            ast::Declaration::Module(md) => {
                module_name = md.name;
            },
            ast::Declaration::Binding(bind) => {
                let b = desugar_binding(&module_name, &mut metadata, bind)?;
                bindings.push(b);
            }
        }
    }
    Ok(Module { metadata, bindings })
}

pub fn desugar_binding(module_name: &Name, metadata: &mut Metadata, binding : ast::Binding) -> Result<Binding> {
    match binding {
        ast::Binding::FunBinding(fb) => desugar_funbinding(module_name, metadata, fb),
        ast::Binding::OpBinding(ob) => desugar_opbinding(module_name, metadata, ob),
        ast::Binding::VarBinding(vb) => desugar_varbinding(module_name, metadata, vb),
    }
}

pub fn desugar_funbinding(module_name: &Name, metadata: &mut Metadata, binding : ast::FunBinding) -> Result<Binding> {
    todo!()
}


pub fn desugar_opbinding(module_name: &Name, metadata: &mut Metadata, binding : ast::OpBinding) -> Result<Binding> {
    todo!()
}


pub fn desugar_varbinding(module_name: &Name, metadata: &mut Metadata, binding : ast::VarBinding) -> Result<Binding> {
    todo!()
}

pub fn desugar_expression(expression : ast::ExpressionNode) -> Result<Expression> {
    todo!()
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
    Var(Node, Var),
    Lambda(Node, Vec<FunClause>),
    App(Node, Box<Expression>, Vec<Expression>),
    Where(Node, Box<Expression>, Vec<Binding>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunClause {
    pub id : Node,
    pub args : Vec<ast::Pattern>,
    pub rhs : Box<Expression>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Var {
    Named(String),
    Local(u32)
}