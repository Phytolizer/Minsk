use crate::analysis::diagnostic::Diagnostic;

use super::lexer::Lexer;
use super::node::expression::ExpressionSyntax;
use super::parser::Parser;
use super::token::SyntaxToken;

pub struct SyntaxTree {
    pub root: ExpressionSyntax,
    pub end_of_file_token: SyntaxToken,
    pub diagnostics: Vec<Diagnostic>,
}

impl SyntaxTree {
    pub(crate) fn new(
        root: ExpressionSyntax,
        end_of_file_token: SyntaxToken,
        diagnostics: Vec<Diagnostic>,
    ) -> Self {
        Self {
            root,
            end_of_file_token,
            diagnostics,
        }
    }

    pub fn parse(text: &str) -> Self {
        Parser::new(text).parse()
    }

    pub fn parse_tokens(text: &str) -> Vec<SyntaxToken> {
        Lexer::new(text).into_only_tokens()
    }
}
