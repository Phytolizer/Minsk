using System.Collections;

namespace Minsk.CodeAnalysis;

public class Lexer : IEnumerable<SyntaxToken?>
{
    private readonly string _text;
    private List<string> _diagnostics = new();
    public IEnumerable<string> Diagnostics => _diagnostics;

    public Lexer(string text)
    {
        _text = text;
    }

    public IEnumerator<SyntaxToken?> GetEnumerator()
    {
        return new Enumerator(_text, _diagnostics);
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return this.GetEnumerator();
    }

    private class Enumerator : IEnumerator<SyntaxToken?>
    {
        private readonly string _text;
        private int _position;
        private List<string> _diagnostics;

        public SyntaxToken? Current { get; private set; }

        object? IEnumerator.Current => this.Current;

        public void Dispose()
        {
        }

        public bool MoveNext()
        {
            Current = Lex();
            return Current.Kind != SyntaxKind.EndOfFileToken;
        }

        public void Reset()
        {
            throw new InvalidOperationException();
        }

        public Enumerator(string text, List<string> diagnostics)
        {
            _text = text;
            _diagnostics = diagnostics;
        }

        private char CurrentChar
        {
            get
            {
                if (_position >= _text.Length)
                {
                    return '\0';
                }

                return _text[_position];
            }
        }

        private string CurrentText(int start) => _text[start.._position];

        private SyntaxToken Lex()
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
                    _diagnostics.Add($"Number {currentText} doesn't fit in {typeof(int)}");
                }

                kind = SyntaxKind.NumberToken;
                value = intVal;
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
                    case ')':
                        kind = SyntaxKind.CloseParenthesisToken;
                        _position++;
                        break;
                    case '\0':
                        kind = SyntaxKind.EndOfFileToken;
                        break;
                    default:
                        _position++;
                        _diagnostics.Add($"Illegal character in input: {_text[start]}");
                        break;
                }
            }

            return new SyntaxToken(kind, currentText ?? CurrentText(start), start, value);
        }
    }
}