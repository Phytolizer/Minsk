module minsk.code_analysis.syntax.facts;

import minsk.code_analysis.syntax.kind : SyntaxKind;

int unaryPrecedence(SyntaxKind kind) {
    switch (kind) {
        case SyntaxKind.PlusToken, SyntaxKind.MinusToken, SyntaxKind.BangToken:
            return 6;
        default:
            return 0;
    }
}

int binaryPrecedence(SyntaxKind kind) {
    switch (kind) {
        case SyntaxKind.StarToken, SyntaxKind.SlashToken:
            return 5;
        case SyntaxKind.PlusToken, SyntaxKind.MinusToken:
            return 4;
        case SyntaxKind.EqualsEqualsToken, SyntaxKind.BangEqualsToken:
            return 3;
        case SyntaxKind.AmpersandAmpersandToken:
            return 2;
        case SyntaxKind.PipePipeToken:
            return 1;
        default:
            return 0;
    }
}

SyntaxKind keywordKind(string text) {
    switch (text) {
        case "true":
            return SyntaxKind.TrueKeyword;
        case "false":
            return SyntaxKind.FalseKeyword;
        default:
            return SyntaxKind.IdentifierToken;
    }
}
