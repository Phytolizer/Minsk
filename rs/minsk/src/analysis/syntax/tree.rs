use crate::analysis::diagnostic::Diagnostic;
use crate::analysis::text::source_text::SourceText;

use super::lexer::Lexer;
use super::node::unit::CompilationUnitSyntax;
use super::parser::Parser;
use super::token::SyntaxToken;

pub struct SyntaxTree {
    pub root: CompilationUnitSyntax,
    pub diagnostics: Vec<Diagnostic>,
    pub source_text: SourceText,
}

impl SyntaxTree {
    fn new(source_text: SourceText) -> Self {
        let mut parser = Parser::new(source_text);
        let root = parser.parse_compilation_unit();
        let (source_text, diagnostics) = parser.unpack();
        Self {
            root,
            diagnostics,
            source_text,
        }
    }

    pub fn parse(text: &str) -> Self {
        Self::new(SourceText::from(text.chars().collect::<Vec<_>>()))
    }

    pub fn parse_tokens(text: &str) -> Vec<SyntaxToken> {
        Lexer::new(&SourceText::from(text.chars().collect::<Vec<_>>())).into_only_tokens()
    }
}
