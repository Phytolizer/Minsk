using System.Collections;

namespace Minsk;

public class Lexer : IEnumerable<SyntaxToken?>
{
    private readonly string _text;

    public Lexer(string text)
    {
        _text = text;
    }

    public IEnumerator<SyntaxToken?> GetEnumerator()
    {
        return new Enumerator(_text);
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return this.GetEnumerator();
    }

    private class Enumerator : IEnumerator<SyntaxToken?>
    {
        private readonly string _text;
        private int _position;

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

        public Enumerator(string text)
        {
            _text = text;
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
                        break;
                }
            }

            return new SyntaxToken(kind, currentText ?? CurrentText(start), start, value);
        }
    }
}
