use std::fmt::Display;

use super::text_span::TextSpan;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    span: TextSpan,
    message: String,
}

impl Diagnostic {
    pub(crate) fn new(span: TextSpan, message: String) -> Self {
        Self { span, message }
    }

    pub fn span(&self) -> &TextSpan {
        &self.span
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}
