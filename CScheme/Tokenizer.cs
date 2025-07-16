using System.Text;

namespace CScheme;

public static class Tokenizer
{
    public abstract record Token;
    public record OpenToken : Token;
    public record CloseToken : Token;
    public record QuoteToken : Token;
    public record UnquoteToken : Token;
    public record NumberToken(string number) : Token;
    public record StringToken(string str) : Token;
    public record SymbolToken(string symbol) : Token;

    public static IEnumerable<Token> Tokenize(string source)
    {
        var chars = source.ToCharArray();
        var i = 0;

        while (i < chars.Length)
        {
            var current = chars[i];
            // Skip whitespace
            if (char.IsWhiteSpace(current))
            {
                i++;
                continue;
            }

            // Skip comments
            if (current == ';')
            {
                ConsumeComment();
                continue;
            }

            switch(current)
            {
                case '(':
                    i++;
                    yield return new OpenToken();
                    break;
                case ')':
                    i++;
                    yield return new CloseToken();
                    break;
                case '\'':
                    i++;
                    yield return new QuoteToken();
                    break;
                case ',':
                    i++;
                    yield return new UnquoteToken();
                    break;
                case '"':
                    yield return new StringToken(ParseString());
                    break;
                case '-' when i + 1 < chars.Length && Char.IsDigit(chars[i + 1]): 
                case '+' when i + 1 < chars.Length && Char.IsDigit(chars[i + 1]):
                    yield return new NumberToken(ParseToken());
                    break;
                default:
                    yield return Char.IsDigit(current)
                        ? new NumberToken(ParseToken())
                        : new SymbolToken(ParseToken());
                    break;
            }
        }

        yield break;

        void ConsumeComment()
        {
            while (i < chars.Length && chars[i] != '\r' && chars[i] != '\n')
            {
                i++;
            }
        }

        string ParseString()
        {
            var sb = new StringBuilder();
            i++;
            while (i < chars.Length)
            {
                if (i + 1 < chars.Length && chars[i] == '\\')
                {
                    // Handle escape sequences with switch expression
                    var escaped = chars[i + 1] switch
                    {
                        '"' => '"',
                        'b' => '\b',
                        'f' => '\f',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        _ => throw new InvalidOperationException("Malformed string: invalid escape sequence")
                    };
                    sb.Append(escaped);
                    i += 2;
                }
                else if (chars[i] == '"')
                {
                    // Closing quote found
                    i++;
                    return sb.ToString();
                }
                else
                {
                    // Regular character
                    sb.Append(chars[i]);
                    i++;
                }
            }

            throw new InvalidOperationException("Malformed string: unterminated string literal");
        }

        string ParseToken()
        {
            var sb = new StringBuilder();

            while (i < chars.Length)
            {
                char current = chars[i];

                // Token terminates on closing paren, whitespace, or EOF
                if (current == ')' || char.IsWhiteSpace(current))
                {
                    break;
                }

                sb.Append(current);
                i++;
            }

            return sb.ToString();
        }
    }
}