using System.Diagnostics.SymbolStore;
using System.Runtime.InteropServices.JavaScript;

namespace CScheme.Tests;

using Xunit;
using System.Linq;

public class TokenizerTests
{
    [Fact]
    public void Tokenize_EmptyString_ReturnsEmptySequence()
    {
        // Arrange
        var input = "";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Empty(tokens);
    }

    [Fact]
    public void Tokenize_WhitespaceOnly_ReturnsEmptySequence()
    {
        // Arrange
        var input = "   \t\n\r  ";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Empty(tokens);
    }

    [Fact]
    public void Tokenize_OpenParen_ReturnsOpenToken()
    {
        // Arrange
        var input = "(";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        Assert.IsType<Tokenizer.OpenToken>(tokens[0]);
    }

    [Fact]
    public void Tokenize_CloseParen_ReturnsCloseToken()
    {
        // Arrange
        var input = ")";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        Assert.IsType<Tokenizer.CloseToken>(tokens[0]);
    }

    [Fact]
    public void Tokenize_Quote_ReturnsQuoteToken()
    {
        // Arrange
        var input = "'";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        Assert.IsType<Tokenizer.QuoteToken>(tokens[0]);
    }

    [Fact]
    public void Tokenize_Unquote_ReturnsUnquoteToken()
    {
        // Arrange
        var input = ",";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        Assert.IsType<Tokenizer.UnquoteToken>(tokens[0]);
    }

    [Fact]
    public void Tokenize_SimpleString_ReturnsStringToken()
    {
        // Arrange
        var input = "\"hello world\"";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        var stringToken = Assert.IsType<Tokenizer.StringToken>(tokens[0]);
        Assert.Equal("hello world", stringToken.str);
    }

    [Fact]
    public void Tokenize_StringWithEscapedQuote_ReturnsCorrectString()
    {
        // Arrange
        var input = "\"hello \\\"world\\\"\"";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        var stringToken = Assert.IsType<Tokenizer.StringToken>(tokens[0]);
        Assert.Equal("hello \"world\"", stringToken.str);
    }

    [Theory]
    [InlineData("\"\\b\\f\\n\\r\\t\\\\\"", "\b\f\n\r\t\\")]
    [InlineData("\"\\n\"", "\n")]
    [InlineData("\"\\t\"", "\t")]
    [InlineData("\"\\\\\"", "\\")]
    public void Tokenize_StringWithEscapeSequences_ReturnsCorrectString(string input, string expected)
    {
        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        var stringToken = Assert.IsType<Tokenizer.StringToken>(tokens[0]);
        Assert.Equal(expected, stringToken.str);
    }

    [Fact]
    public void Tokenize_UnterminatedString_ThrowsException()
    {
        // Arrange
        var input = "\"hello world";

        // Act & Assert
        var exception = Assert.Throws<InvalidOperationException>(() => 
            Tokenizer.Tokenize(input).ToList());
        Assert.Contains("unterminated string literal", exception.Message);
    }

    [Fact]
    public void Tokenize_InvalidEscapeSequence_ThrowsException()
    {
        // Arrange
        var input = "\"hello \\x world\"";

        // Act & Assert
        var exception = Assert.Throws<InvalidOperationException>(() => 
            Tokenizer.Tokenize(input).ToList());
        Assert.Contains("invalid escape sequence", exception.Message);
    }

    [Theory]
    [InlineData("123", "123")]
    [InlineData("0", "0")]
    [InlineData("999", "999")]
    public void Tokenize_PositiveNumber_ReturnsNumberToken(string input, string expected)
    {
        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        var numberToken = Assert.IsType<Tokenizer.NumberToken>(tokens[0]);
        Assert.Equal(expected, numberToken.number);
    }

    [Theory]
    [InlineData("-456", "-456")]
    [InlineData("-0", "-0")]
    [InlineData("-999", "-999")]
    public void Tokenize_NegativeNumber_ReturnsNumberToken(string input, string expected)
    {
        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        var numberToken = Assert.IsType<Tokenizer.NumberToken>(tokens[0]);
        Assert.Equal(expected, numberToken.number);
    }

    [Theory]
    [InlineData("+789", "+789")]
    [InlineData("+0", "+0")]
    [InlineData("+123", "+123")]
    public void Tokenize_ExplicitPositiveNumber_ReturnsNumberToken(string input, string expected)
    {
        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        var numberToken = Assert.IsType<Tokenizer.NumberToken>(tokens[0]);
        Assert.Equal(expected, numberToken.number);
    }

    [Theory]
    [InlineData("hello-world", "hello-world")]
    [InlineData("foo", "foo")]
    [InlineData("camelCase", "camelCase")]
    [InlineData("snake_case", "snake_case")]
    [InlineData("kebab-case", "kebab-case")]
    public void Tokenize_Symbol_ReturnsSymbolToken(string input, string expected)
    {
        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        var symbolToken = Assert.IsType<Tokenizer.SymbolToken>(tokens[0]);
        Assert.Equal(expected, symbolToken.symbol);
    }

    [Theory]
    [InlineData("-")]
    [InlineData("+")]
    public void Tokenize_SignAsSymbol_ReturnsSymbolToken(string input)
    {
        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        var symbolToken = Assert.IsType<Tokenizer.SymbolToken>(tokens[0]);
        Assert.Equal(input, symbolToken.symbol);
    }

    [Fact]
    public void Tokenize_CommentLine_SkipsComment()
    {
        // Arrange
        var input = "; this is a comment\n123";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        var numberToken = Assert.IsType<Tokenizer.NumberToken>(tokens[0]);
        Assert.Equal("123", numberToken.number);
    }

    [Fact]
    public void Tokenize_CommentAtEndOfFile_SkipsComment()
    {
        // Arrange
        var input = "123 ; this is a comment";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Single(tokens);
        Assert.IsType<Tokenizer.NumberToken>(tokens[0]);
    }

    [Fact]
    public void Tokenize_MultipleTokens_ReturnsCorrectSequence()
    {
        // Arrange
        var input = "(+ 1 2)";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Equal(5, tokens.Count);
        Assert.IsType<Tokenizer.OpenToken>(tokens[0]);
        var symbolToken = Assert.IsType<Tokenizer.SymbolToken>(tokens[1]);
        Assert.Equal("+", symbolToken.symbol);
        var num1 = Assert.IsType<Tokenizer.NumberToken>(tokens[2]);
        Assert.Equal("1", num1.number);
        var num2 = Assert.IsType<Tokenizer.NumberToken>(tokens[3]);
        Assert.Equal("2", num2.number);
        Assert.IsType<Tokenizer.CloseToken>(tokens[4]);
    }

    [Fact]
    public void Tokenize_ComplexExpression_ReturnsCorrectSequence()
    {
        // Arrange
        var input = "'(foo \"bar\" -123 +456)";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Equal(7, tokens.Count);
        Assert.IsType<Tokenizer.QuoteToken>(tokens[0]);
        Assert.IsType<Tokenizer.OpenToken>(tokens[1]);
        var symbolToken = Assert.IsType<Tokenizer.SymbolToken>(tokens[2]);
        Assert.Equal("foo", symbolToken.symbol);
        var stringToken = Assert.IsType<Tokenizer.StringToken>(tokens[3]);
        Assert.Equal("bar", stringToken.str);
        var num1 = Assert.IsType<Tokenizer.NumberToken>(tokens[4]);
        Assert.Equal("-123", num1.number);
        var num2 = Assert.IsType<Tokenizer.NumberToken>(tokens[5]);
        Assert.Equal("+456", num2.number);
        Assert.IsType<Tokenizer.CloseToken>(tokens[6]);
    }

    [Fact]
    public void Tokenize_TokenTerminatedByParen_ParsesCorrectly()
    {
        // Arrange
        var input = "(abc)";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Equal(3, tokens.Count);
        Assert.IsType<Tokenizer.OpenToken>(tokens[0]);
        var symbolToken = Assert.IsType<Tokenizer.SymbolToken>(tokens[1]);
        Assert.Equal("abc", symbolToken.symbol);
        Assert.IsType<Tokenizer.CloseToken>(tokens[2]);
    }

    [Theory]
    [InlineData("abc def", new[] { "abc", "def" })]
    [InlineData("one two three", new[] { "one", "two", "three" })]
    public void Tokenize_TokensTerminatedByWhitespace_ParsesCorrectly(string input, string[] expectedSymbols)
    {
        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Equal(expectedSymbols.Length, tokens.Count);
        for (int i = 0; i < expectedSymbols.Length; i++)
        {
            var symbolToken = Assert.IsType<Tokenizer.SymbolToken>(tokens[i]);
            Assert.Equal(expectedSymbols[i], symbolToken.symbol);
        }
    }

    [Theory]
    [InlineData("  ( \t\n ) \r\n ")]
    [InlineData("(\n)")]
    [InlineData("( )")]
    public void Tokenize_MixedWhitespace_HandlesCorrectly(string input)
    {
        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Equal(2, tokens.Count);
        Assert.IsType<Tokenizer.OpenToken>(tokens[0]);
        Assert.IsType<Tokenizer.CloseToken>(tokens[1]);
    }

    [Fact]
    public void Tokenize_EmptyParens_ReturnsCorrectTokens()
    {
        // Arrange
        var input = "()";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Equal(2, tokens.Count);
        Assert.IsType<Tokenizer.OpenToken>(tokens[0]);
        Assert.IsType<Tokenizer.CloseToken>(tokens[1]);
    }

    [Fact]
    public void Tokenize_UnquoteOperator_ParsesCorrectly()
    {
        // Arrange
        var input = ",foo";

        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Equal(2, tokens.Count);
        Assert.IsType<Tokenizer.UnquoteToken>(tokens[0]);
        var symbolToken = Assert.IsType<Tokenizer.SymbolToken>(tokens[1]);
        Assert.Equal("foo", symbolToken.symbol);
    }

    [Theory]
    [InlineData("()", 2)]
    [InlineData("(())", 4)]
    [InlineData("((()))", 6)]
    public void Tokenize_NestedParens_ReturnsCorrectCount(string input, int expectedCount)
    {
        // Act
        var tokens = Tokenizer.Tokenize(input).ToList();

        // Assert
        Assert.Equal(expectedCount, tokens.Count);
        Assert.Equal(expectedCount / 2, tokens.Count(t => t is Tokenizer.OpenToken));
        Assert.Equal(expectedCount / 2, tokens.Count(t => t is Tokenizer.CloseToken));
    }
}
