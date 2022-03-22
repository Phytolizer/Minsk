use strum::IntoEnumIterator;

use super::kind::SyntaxKind;

pub(crate) fn keyword_kind(text: &str) -> SyntaxKind {
    match text {
        "true" => SyntaxKind::TrueKeyword,
        "false" => SyntaxKind::FalseKeyword,
        _ => SyntaxKind::IdentifierToken,
    }
}

pub(crate) fn binary_operator_precedence(kind: SyntaxKind) -> usize {
    match kind {
        SyntaxKind::StarToken | SyntaxKind::SlashToken => 5,
        SyntaxKind::PlusToken | SyntaxKind::MinusToken => 4,
        SyntaxKind::EqualsEqualsToken | SyntaxKind::BangEqualsToken => 3,
        SyntaxKind::AmpersandAmpersandToken => 2,
        SyntaxKind::PipePipeToken => 1,
        _ => 0,
    }
}

pub(crate) fn unary_operator_precedence(kind: SyntaxKind) -> usize {
    match kind {
        SyntaxKind::BangToken | SyntaxKind::PlusToken | SyntaxKind::MinusToken => 6,
        _ => 0,
    }
}

pub fn get_text(kind: SyntaxKind) -> Option<&'static str> {
    match kind {
        SyntaxKind::BangToken => Some("!"),
        SyntaxKind::BangEqualsToken => Some("!="),
        SyntaxKind::EqualsToken => Some("="),
        SyntaxKind::MinusToken => Some("-"),
        SyntaxKind::PlusToken => Some("+"),
        SyntaxKind::StarToken => Some("*"),
        SyntaxKind::SlashToken => Some("/"),
        SyntaxKind::AmpersandAmpersandToken => Some("&&"),
        SyntaxKind::PipePipeToken => Some("||"),
        SyntaxKind::OpenParenthesisToken => Some("("),
        SyntaxKind::CloseParenthesisToken => Some(")"),
        SyntaxKind::EqualsEqualsToken => Some("=="),
        _ => None,
    }
}

pub fn get_unary_operator_kinds() -> impl Iterator<Item = SyntaxKind> {
    SyntaxKind::iter().filter(|k| unary_operator_precedence(*k) > 0)
}

pub fn get_binary_operator_kinds() -> impl Iterator<Item = SyntaxKind> {
    SyntaxKind::iter().filter(|k| binary_operator_precedence(*k) > 0)
}

#[cfg(test)]
mod tests {
    use strum::IntoEnumIterator;

    use crate::analysis::syntax::kind::SyntaxKind;
    use crate::analysis::syntax::tree::SyntaxTree;

    use super::get_text;

    #[test]
    fn get_text_round_trips() {
        for kind in SyntaxKind::iter()
            .filter(|k| get_text(*k).is_some())
            .collect::<Vec<_>>()
        {
            let text = get_text(kind).unwrap();
            let tokens = SyntaxTree::parse_tokens(text);
            assert_eq!(1, tokens.len());
            assert_eq!(kind, tokens[0].kind);
            assert_eq!(text, tokens[0].text);
        }
    }
}
