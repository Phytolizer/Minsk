use std::fmt::Display;

use crate::analysis::syntax::kind::SyntaxKind;
use crate::analysis::syntax::token::SyntaxToken;

use super::expression::ExpressionSyntax;
use super::SyntaxNode;

#[derive(Debug)]
pub struct CompilationUnitSyntax {
    pub expression: ExpressionSyntax,
    pub end_of_file_token: SyntaxToken,
}

impl Display for CompilationUnitSyntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CompilationUnit")
    }
}

impl SyntaxNode for CompilationUnitSyntax {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::CompilationUnit
    }

    fn children(&self) -> Vec<&dyn SyntaxNode> {
        vec![&self.expression, &self.end_of_file_token]
    }
}
impl CompilationUnitSyntax {
    pub fn new(expression: ExpressionSyntax, end_of_file_token: SyntaxToken) -> Self {
        Self {
            expression,
            end_of_file_token,
        }
    }
}
