use std::fmt::Display;

use crate::analysis::text_span::TextSpan;
use crate::object::Object;

use super::kind::SyntaxKind;
use super::node::SyntaxNode;

#[derive(Debug, Clone)]
pub struct SyntaxToken {
    pub(crate) kind: SyntaxKind,
    pub(crate) text: String,
    pub(crate) position: usize,
    pub(crate) length: usize,
    pub(crate) value: Option<Object>,
}

impl SyntaxToken {
    pub(crate) fn new(
        kind: SyntaxKind,
        text: String,
        position: usize,
        length: usize,
        value: Option<Object>,
    ) -> Self {
        Self {
            kind,
            text,
            position,
            length,
            value,
        }
    }

    pub(crate) fn span(&self) -> TextSpan {
        TextSpan::new(self.position, self.text.len())
    }

    pub(crate) fn position(&self) -> usize {
        self.position
    }

    pub(crate) fn length(&self) -> usize {
        self.length
    }

    pub(crate) fn value(&self) -> Option<&Object> {
        self.value.as_ref()
    }

    pub(crate) fn text(&self) -> &str {
        self.text.as_str()
    }
}

impl Display for SyntaxToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} '{}'", self.kind, self.text)?;
        match self.value.as_ref() {
            Some(v) => write!(f, " {}", v)?,
            None => {}
        }
        Ok(())
    }
}

impl SyntaxNode for SyntaxToken {
    fn kind(&self) -> SyntaxKind {
        self.kind
    }

    fn children(&self) -> Vec<&dyn SyntaxNode> {
        vec![]
    }

    fn is_token(&self) -> bool {
        true
    }
}
