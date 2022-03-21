use std::collections::HashMap;

use crate::object::Object;

use super::binding::binder::Binder;
use super::diagnostic::Diagnostic;
use super::evaluator::Evaluator;
use super::syntax::tree::SyntaxTree;

pub struct Compilation {
    syntax_tree: SyntaxTree,
}

impl Compilation {
    pub fn new(syntax_tree: SyntaxTree) -> Self {
        Self { syntax_tree }
    }

    pub fn evaluate(
        self,
        variables: &mut HashMap<String, Object>,
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
