use std::collections::HashMap;

use crate::object::Object;

use super::binding::binder::Binder;
use super::diagnostic::Diagnostic;
use super::evaluator::Evaluator;
use super::syntax::tree::SyntaxTree;
use super::variable_symbol::VariableSymbol;

pub struct Compilation<'t> {
    syntax_tree: &'t mut SyntaxTree,
}

impl<'t> Compilation<'t> {
    pub fn new(syntax_tree: &'t mut SyntaxTree) -> Self {
        Self { syntax_tree }
    }

    pub fn evaluate(
        self,
        variables: &mut HashMap<VariableSymbol, Object>,
    ) -> Result<Object, Vec<Diagnostic>> {
        let (bound_expression, diagnostics) =
            Binder::new(variables).into_expression_and_diagnostics(self.syntax_tree);
        if !diagnostics.is_empty() {
            Err(diagnostics)
        } else {
            let evaluator = Evaluator::new(variables);
            Ok(evaluator.evaluate(bound_expression))
        }
    }
}
