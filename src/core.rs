
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
                let b = state.desugar_binding(bind)?;
                state.bindings.push(b);
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
    fn desugar_binding(&mut self, binding : ast::Binding) -> Result<Binding> {
        match binding {
            ast::Binding::FunBinding(fb) => self.desugar_funbinding(fb),
            ast::Binding::OpBinding(ob) => self.desugar_opbinding(ob),
            ast::Binding::VarBinding(vb) => self.desugar_varbinding(vb),
        }
    }

    fn desugar_funbinding(&mut self, binding : ast::FunBinding) -> Result<Binding> {
        todo!()
    }


    fn desugar_opbinding(&mut self, binding : ast::OpBinding) -> Result<Binding> {
        todo!()
    }


    fn desugar_varbinding(&mut self, binding : ast::VarBinding) -> Result<Binding> {
        todo!()
    }

    fn desugar_expression(&mut self, expression : ast::ExpressionNode) -> Result<Expression> {
        todo!()
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