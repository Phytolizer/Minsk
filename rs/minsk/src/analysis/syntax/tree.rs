use crate::analysis::diagnostic::Diagnostic;
use crate::analysis::text::source_text::SourceText;

use super::lexer::Lexer;
use super::node::expression::ExpressionSyntax;
use super::parser::Parser;
use super::token::SyntaxToken;

pub struct SyntaxTree {
    pub root: ExpressionSyntax,
    pub end_of_file_token: SyntaxToken,
    pub diagnostics: Vec<Diagnostic>,
    pub source_text: SourceText,
}

impl SyntaxTree {
    pub(crate) fn new(
        root: ExpressionSyntax,
        end_of_file_token: SyntaxToken,
        diagnostics: Vec<Diagnostic>,
        source_text: SourceText,
    ) -> Self {
        Self {
            root,
            end_of_file_token,
            diagnostics,
            source_text,
        }
    }

    pub fn parse(text: &str) -> Self {
        Parser::new(SourceText::from(text.chars().collect::<Vec<_>>())).parse()
    }

    pub fn parse_tokens(text: &str) -> Vec<SyntaxToken> {
        Lexer::new(&SourceText::from(text.chars().collect::<Vec<_>>())).into_only_tokens()
    }
}
