namespace Minsk;

public class Lexer
{
    private readonly string _text;
    private int _position = 0;

    private char Current => _position >= _text.Length ? '\0' : _text[_position];

    public Lexer(string text)
    {
        _text = text;
    }

    public SyntaxToken NextToken()
    {
        var kind = SyntaxKind.BadToken;
        var start = _position;
        object? value = null;

        if (char.IsDigit(Current))
        {
            while (char.IsDigit(Current))
            {
                ++_position;
            }

            kind = SyntaxKind.NumberToken;
            if (!int.TryParse(_text[start.._position], out var intVal))
            {
                throw new NotImplementedException();
            }

            value = intVal;
        }
        else if (char.IsWhiteSpace(Current))
        {
            while (char.IsWhiteSpace(Current))
            {
                ++_position;
            }

            kind = SyntaxKind.WhitespaceToken;
        }
        else
        {
            switch (Current)
            {
                case '+':
                    ++_position;
                    kind = SyntaxKind.PlusToken;
                    break;
                case '-':
                    ++_position;
                    kind = SyntaxKind.MinusToken;
                    break;
                case '*':
                    ++_position;
                    kind = SyntaxKind.StarToken;
                    break;
                case '/':
                    ++_position;
                    kind = SyntaxKind.SlashToken;
                    break;
                case '(':
                    ++_position;
                    kind = SyntaxKind.OpenParenthesisToken;
                    break;
                case ')':
                    ++_position;
                    kind = SyntaxKind.CloseParenthesisToken;
                    break;
                case '\0':
                    kind = SyntaxKind.EndOfFileToken;
                    break;
                default:
                    ++_position;
                    break;
            }
        }

        return new SyntaxToken(start, _text[start.._position], kind, value);
    }
}