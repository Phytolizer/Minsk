module minsk.code_analysis.syntax.facts;

import minsk.code_analysis.syntax.kind : SyntaxKind;

int unaryPrecedence(SyntaxKind kind) {
    switch (kind) {
        case SyntaxKind.PlusToken, SyntaxKind.MinusToken:
            return 3;
        default:
            return 0;
    }
}

int binaryPrecedence(SyntaxKind kind) {
    switch (kind) {
        case SyntaxKind.StarToken, SyntaxKind.SlashToken:
            return 2;
        case SyntaxKind.PlusToken, SyntaxKind.MinusToken:
            return 1;
        default:
            return 0;
    }
}
