module minsk.code_analysis.syntax.parser;

import std.algorithm : among, filter;
import std.range : InputRange, inputRangeObject;

import minsk.code_analysis.syntax.lexer : Lexer;
import minsk.code_analysis.syntax.syntax_token : SyntaxToken;
import minsk.code_analysis.syntax.syntax_kind : SyntaxKind;

class Parser {
    InputRange!SyntaxToken _tokens;

    this(string text) {
        _tokens = new Lexer(text)
            .filter!(tok => !tok.kind.among(
                    SyntaxKind.BadToken,
                    SyntaxKind.WhitespaceToken,
            ))
            .inputRangeObject;
    }
}
