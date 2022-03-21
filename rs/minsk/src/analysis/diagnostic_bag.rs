use crate::object::ObjectKind;

use super::diagnostic::Diagnostic;
use super::syntax::kind::SyntaxKind;
use super::text_span::TextSpan;

pub(crate) struct DiagnosticBag {
    diagnostics: Vec<Diagnostic>,
}

impl DiagnosticBag {
    pub(crate) fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    fn report(&mut self, span: TextSpan, message: String) {
        self.diagnostics.push(Diagnostic::new(span, message));
    }

    pub(crate) fn report_bad_character(&mut self, position: usize, c: char) {
        self.report(
            TextSpan::new(position, 1),
            format!("Bad character in input: {}", c),
        );
    }

    pub(crate) fn report_invalid_i64(&mut self, span: TextSpan, text: &str) {
        self.report(span, format!("The number {} doesn't fit in i64", text));
    }

    pub(crate) fn report_unexpected_token(
        &mut self,
        span: TextSpan,
        expected_kind: SyntaxKind,
        actual_kind: SyntaxKind,
    ) {
        self.report(
            span,
            format!(
                "Expected next token to be {:?}, got {:?} instead",
                expected_kind, actual_kind
            ),
        );
    }

    pub(crate) fn report_undefined_binary_operator(
        &mut self,
        span: TextSpan,
        operator: &str,
        left_type: ObjectKind,
        right_type: ObjectKind,
    ) {
        self.report(
            span,
            format!(
                "Binary operator {} isn't defined for types {:?} and {:?}",
                operator, left_type, right_type
            ),
        );
    }

    pub(crate) fn report_undefined_unary_operator(
        &mut self,
        span: TextSpan,
        operator: &str,
        operand_type: ObjectKind,
    ) {
        self.report(
            span,
            format!(
                "Unary operator {} isn't defined for type {:?}",
                operator, operand_type
            ),
        );
    }

    pub(crate) fn report_undefined_name(&mut self, span: TextSpan, name: &str) {
        self.report(span, format!("Undefined name '{}'", name));
    }
}

pub(crate) struct DiagnosticBagIterator {
    diagnostics: Vec<Diagnostic>,
    position: usize,
}

impl Iterator for DiagnosticBagIterator {
    type Item = Diagnostic;

    fn next(&mut self) -> Option<Self::Item> {
        let diag = match self.diagnostics.get(self.position) {
            Some(d) => d,
            None => return None,
        };
        self.position += 1;
        Some(diag.clone())
    }
}

impl IntoIterator for DiagnosticBag {
    type IntoIter = DiagnosticBagIterator;
    type Item = <Self::IntoIter as Iterator>::Item;

    fn into_iter(self) -> Self::IntoIter {
        DiagnosticBagIterator {
            diagnostics: self.diagnostics,
            position: 0,
        }
    }
}
