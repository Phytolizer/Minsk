module minsk.code_analysis.syntax.lexer;

import std.ascii : isDigit, isWhite;
import std.conv : to, ConvException;
import std.format : format;

import minsk.runtime.object : Integer, Obj;
import minsk.code_analysis.syntax.syntax_kind : SyntaxKind;
import minsk.code_analysis.syntax.syntax_token : SyntaxToken;

import optional : no, or, some;

final class Lexer {
    private const(string) _text;
    private int _position = 0;
    private SyntaxToken _token;
    private bool _atEof = false;
    bool empty = false;
    private string[] _diagnostics = [];

    this(string text) {
        _text = text;
        _token = nextToken();
    }

    private char current() {
        return _position >= _text.length ? '\0' : _text[_position];
    }

    private SyntaxToken nextToken() {
        auto kind = SyntaxKind.BadToken;
        const start = _position;
        auto text = no!string;
        Obj value = null;
        if (_atEof) {
            empty = true;
            return _token;
        }

        if (isDigit(current)) {
            while (isDigit(current)) {
                _position++;
            }

            text = _text[start .. _position];
            try {
                value = new Integer(text.front.to!int);
            } catch (ConvException) {
                _diagnostics ~= format!"The number '%s' isn't a valid int."(text.front);
            }
            kind = SyntaxKind.NumberToken;
        } else if (isWhite(current)) {
            while (isWhite(current)) {
                _position++;
            }

            text = _text[start .. _position];
            kind = SyntaxKind.WhitespaceToken;
        } else {
            switch (current) {
                case '+':
                    _position++;
                    kind = SyntaxKind.PlusToken;
                    break;
                case '-':
                    _position++;
                    kind = SyntaxKind.MinusToken;
                    break;
                case '*':
                    _position++;
                    kind = SyntaxKind.StarToken;
                    break;
                case '/':
                    _position++;
                    kind = SyntaxKind.SlashToken;
                    break;
                case '(':
                    _position++;
                    kind = SyntaxKind.OpenParenthesisToken;
                    break;
                case ')':
                    _position++;
                    kind = SyntaxKind.CloseParenthesisToken;
                    break;
                case '\0':
                    kind = SyntaxKind.EndOfFileToken;
                    _atEof = true;
                    break;
                default:
                    _diagnostics ~= format!"ERROR: bad character in input: '%c'"(current);
                    _position++;
                    break;
            }
        }

        return new SyntaxToken(
            kind,
            start,
            text.or(some(_text[start .. _position])).front,
            value,
        );
    }

    SyntaxToken front() {
        return _token;
    }

    void popFront() {
        _token = nextToken();
    }

    const(string[]) diagnostics() const {
        return _diagnostics;
    }
}
