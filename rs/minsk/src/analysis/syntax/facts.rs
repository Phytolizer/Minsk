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
