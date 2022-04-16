using System.Collections;
using Minsk.CodeAnalysis.Text;

namespace Minsk.CodeAnalysis.Syntax;

internal sealed class Lexer : IEnumerable<SyntaxToken>
{
    private readonly DiagnosticBag _diagnostics = new();
    private readonly SourceText _text;
    private int _position;

    public Lexer(SourceText text)
    {
        _text = text;
    }

    public IEnumerable<Diagnostic> Diagnostics => _diagnostics;

    private char CurrentChar => Peek(0);

    public IEnumerator<SyntaxToken> GetEnumerator()
    {
        while (true)
        {
            var start = _position;
            var kind = SyntaxKind.BadToken;
            string? currentText = null;
            object? value = null;

            if (char.IsWhiteSpace(CurrentChar))
            {
                while (char.IsWhiteSpace(CurrentChar))
                {
                    _position++;
                }

                kind = SyntaxKind.WhitespaceToken;
            }
            else if (char.IsDigit(CurrentChar))
            {
                while (char.IsDigit(CurrentChar))
                {
                    _position++;
                }

                currentText = CurrentText(start);
                if (!int.TryParse(currentText, out var intVal))
                {
                    _diagnostics.ReportInvalidInt(new TextSpan(start, _position - start), currentText, typeof(int));
                }

                kind = SyntaxKind.NumberToken;
                value = intVal;
            }
            else if (char.IsLetter(CurrentChar))
            {
                while (char.IsLetterOrDigit(CurrentChar))
                {
                    _position++;
                }

                currentText = CurrentText(start);
                kind = SyntaxFacts.GetKeywordKind(currentText);
            }
            else
            {
                switch (CurrentChar)
                {
                    case '+':
                        kind = SyntaxKind.PlusToken;
                        _position++;
                        break;
                    case '-':
                        kind = SyntaxKind.MinusToken;
                        _position++;
                        break;
                    case '*':
                        kind = SyntaxKind.StarToken;
                        _position++;
                        break;
                    case '/':
                        kind = SyntaxKind.SlashToken;
                        _position++;
                        break;
                    case '(':
                        kind = SyntaxKind.OpenParenthesisToken;
                        _position++;
                        break;
                    case '{':
                        kind = SyntaxKind.OpenBraceToken;
                        _position++;
                        break;
                    case '}':
                        kind = SyntaxKind.CloseBraceToken;
                        _position++;
                        break;
                    case ')':
                        kind = SyntaxKind.CloseParenthesisToken;
                        _position++;
                        break;
                    case '!':
                        if (Peek(1) == '=')
                        {
                            kind = SyntaxKind.BangEqualsToken;
                            _position += 2;
                        }
                        else
                        {
                            kind = SyntaxKind.BangToken;
                            _position++;
                        }

                        break;
                    case '=':
                        if (Peek(1) == '=')
                        {
                            kind = SyntaxKind.EqualsEqualsToken;
                            _position += 2;
                        }
                        else
                        {
                            kind = SyntaxKind.EqualsToken;
                            _position++;
                        }

                        break;
                    case '<':
                        if (Peek(1) == '=')
                        {
                            kind = SyntaxKind.LessEqualsToken;
                            _position += 2;
                        }
                        else
                        {
                            kind = SyntaxKind.LessToken;
                            _position++;
                        }

                        break;
                    case '>':
                        if (Peek(1) == '=')
                        {
                            kind = SyntaxKind.GreaterEqualsToken;
                            _position += 2;
                        }
                        else
                        {
                            kind = SyntaxKind.GreaterToken;
                            _position++;
                        }

                        break;
                    case '&':
                        if (Peek(1) == '&')
                        {
                            kind = SyntaxKind.AmpersandAmpersandToken;
                            _position += 2;
                        }

                        break;
                    case '|':
                        if (Peek(1) == '|')
                        {
                            kind = SyntaxKind.PipePipeToken;
                            _position += 2;
                        }

                        break;
                    case '\0':
                        yield break;
                }
            }

            if (kind == SyntaxKind.BadToken)
            {
                _diagnostics.ReportBadCharacter(_position, CurrentChar);
                _position++;
            }

            yield return new SyntaxToken(kind, currentText ?? CurrentText(start), start, value);
        }
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }

    private char Peek(int offset)
    {
        var index = _position + offset;
        if (index >= _text.Length)
        {
            return '\0';
        }

        return _text[index];
    }

    private string CurrentText(int start)
    {
        return _text.ToString(start, _position - start);
    }
}
