using System.Reflection;
using System.Text;
using CScheme;
using static CScheme.Scheme;
using Environment = System.Collections.Immutable.ImmutableDictionary<string, CScheme.Expression>;

var env = InitializeEnvironment();
Console.WriteLine("Welcome to CScheme");
Repl(env);

static Environment InitializeEnvironment()
{
    var (env, _) = Load(Env, "Stdlib.scm");
    
    // Register native functions
    env = DefineNativeFunction("now", () => DateTime.Now, env);
    
    using var httpClient = new HttpClient();
    env = DefineNativeFunction<string>("http-get", uri => httpClient.GetStringAsync(uri).GetAwaiter().GetResult(), env);
    
    env = DefineNativeFunction<string, object>("__get", (method, instance) =>
        instance.GetType().InvokeMember(
            method,
            BindingFlags.GetProperty | BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase,
            null,
            instance,
            Array.Empty<object>()
        )!, 
        env);
        
    return env;
}

/// <summary>
/// Main REPL (Read-Eval-Print Loop) for the interpreter
/// </summary>
static void Repl(Environment env)
{
    var reader = new ConsoleInputReader();
    const string PROMPT = "> ";
    
    // Special commands
    var commands = new Dictionary<string, Func<Environment, (Environment, string)>>(StringComparer.OrdinalIgnoreCase)
    {
        [".tests"] = e => { var (newEnv, _) = Load(Env, "Tests.scm"); return (newEnv, "\n"); },
        [".trace"] = e => { Trace = true; return (e, "Trace on"); },
        [".notrace"] = e => { Trace = false; return (e, "Trace off"); },
        [".help"] = e => (e, "Available commands: .tests, .trace, .notrace, .help, .exit"),
        [".exit"] = e => { Console.WriteLine("Bye!"); System.Environment.Exit(0); return (e, string.Empty); }
    };
    
    while (true)
    {
        Console.Write(PROMPT);
        var input = reader.ReadLine();
        
        if (string.IsNullOrWhiteSpace(input))
            continue;
            
        try
        {
            // Check for special commands
            if (commands.TryGetValue(input, out var command))
            {
                var (newEnv, message) = command(env);
                env = newEnv;
                
                if (!string.IsNullOrEmpty(message))
                    Console.WriteLine(message);
                    
                continue;
            }
            
            // Parse and evaluate the input
            var tokens = Tokenizer.Tokenize(input).ToArray();
            var parsed = Parse(tokens);
            
            if (parsed.Length == 0)
                continue;
                
            var (newEnvP, expressionResult) = Eval(env, parsed[0]);
            var result = Print(expressionResult);
            
            if (!string.IsNullOrWhiteSpace(result))
                Console.WriteLine(result);
                
            env = newEnvP;
        }
        catch (Exception ex)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            Console.WriteLine($"*** ERROR: {ex.Message}");
            Console.ResetColor();
        }
    }
}

/// <summary>
/// Enhanced console input reader with history navigation and multi-line editing
/// </summary>
public class ConsoleInputReader
{
    private readonly List<string> _history = [];
    private int _historyIndex = -1;
    private const string NEWLINE = "\n";

    public string ReadLine()
    {
        var input = new StringBuilder();
        var cursorPosition = 0;
        var currentLine = 0;

        // Save initial cursor position
        var initialCursorLeft = Console.CursorLeft;
        var initialCursorTop = Console.CursorTop;
        
        while (true)
        {
            var keyInfo = Console.ReadKey(true);

            switch (keyInfo.Key)
            {
                case ConsoleKey.Enter when keyInfo.Modifiers.HasFlag(ConsoleModifiers.Alt):
                    HandleNewLine(input, ref cursorPosition, ref currentLine, ref initialCursorTop, initialCursorLeft);
                    break;
                    
                case ConsoleKey.Enter:
                    return HandleSubmit(input);
                    
                case ConsoleKey.Backspace when cursorPosition > 0:
                    HandleBackspace(input, ref cursorPosition, ref currentLine, initialCursorLeft, initialCursorTop);
                    break;
                    
                case ConsoleKey.Delete when cursorPosition < input.Length:
                    HandleDelete(input, cursorPosition, initialCursorLeft, initialCursorTop);
                    break;
                    
                case ConsoleKey.LeftArrow when cursorPosition > 0:
                    HandleLeftArrow(input, ref cursorPosition, ref currentLine, initialCursorLeft, initialCursorTop);
                    break;
                    
                case ConsoleKey.RightArrow when cursorPosition < input.Length:
                    HandleRightArrow(input, ref cursorPosition, ref currentLine, initialCursorLeft, initialCursorTop);
                    break;
                    
                case ConsoleKey.UpArrow:
                    HandleUpArrow(input, ref cursorPosition, ref currentLine, initialCursorLeft, initialCursorTop);
                    break;
                    
                case ConsoleKey.DownArrow:
                    HandleDownArrow(input, ref cursorPosition, ref currentLine, initialCursorLeft, initialCursorTop);
                    break;
                    
                case ConsoleKey.Home:
                    HandleHome(input, ref cursorPosition, ref currentLine, initialCursorLeft, initialCursorTop);
                    break;
                    
                case ConsoleKey.End:
                    HandleEnd(input, ref cursorPosition, ref currentLine, initialCursorLeft, initialCursorTop);
                    break;
                    
                default:
                    if (!char.IsControl(keyInfo.KeyChar))
                    {
                        input.Insert(cursorPosition, keyInfo.KeyChar);
                        cursorPosition++;
                        RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
                    }
                    break;
            }
        }
    }
    
    private void HandleNewLine(StringBuilder input, ref int cursorPosition, ref int currentLine, 
                              ref int initialCursorTop, int initialCursorLeft)
    {
        input.Insert(cursorPosition, NEWLINE);
        cursorPosition += NEWLINE.Length;
        currentLine++;
        
        // Update console display
        Console.WriteLine();
        
        // Adjust initialCursorTop if we're at the bottom of the buffer
        if (Console.CursorTop == Console.BufferHeight - 1)
            initialCursorTop = Console.BufferHeight - currentLine - 1;
            
        RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
    }
    
    private string HandleSubmit(StringBuilder input)
    {
        Console.WriteLine();
        string result = input.ToString();
        
        // Add to history if not empty and not a duplicate of the last entry
        if (!string.IsNullOrEmpty(result) && 
            (_history.Count == 0 || result != _history[^1]))
        {
            _history.Add(result);
        }
        
        _historyIndex = _history.Count;
        return result;
    }
    
    private void HandleBackspace(StringBuilder input, ref int cursorPosition, ref int currentLine, 
                                int initialCursorLeft, int initialCursorTop)
    {
        cursorPosition--;
        input.Remove(cursorPosition, 1);
        
        // Check if we deleted a newline
        if (cursorPosition > 0 && 
            input.Length >= cursorPosition && 
            input.ToString().Substring(Math.Max(0, cursorPosition - 1), 1) == NEWLINE)
        {
            currentLine--;
        }
        
        RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
    }
    
    private void HandleDelete(StringBuilder input, int cursorPosition, int initialCursorLeft, int initialCursorTop)
    {
        input.Remove(cursorPosition, 1);
        RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
    }
    
    private void HandleLeftArrow(StringBuilder input, ref int cursorPosition, ref int currentLine, 
                               int initialCursorLeft, int initialCursorTop)
    {
        cursorPosition--;
        
        // Check if we crossed a line boundary
        if (cursorPosition > 0 && 
            input.ToString().Substring(Math.Max(0, cursorPosition - 1), 1) == NEWLINE)
        {
            currentLine--;
        }
        
        RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
    }
    
    private void HandleRightArrow(StringBuilder input, ref int cursorPosition, ref int currentLine, 
                                int initialCursorLeft, int initialCursorTop)
    {
        cursorPosition++;
        
        // Check if we crossed a line boundary
        if (cursorPosition < input.Length && 
            input.ToString().Substring(Math.Max(0, cursorPosition - 1), 1) == NEWLINE)
        {
            currentLine++;
        }
        
        RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
    }
    
    private void HandleUpArrow(StringBuilder input, ref int cursorPosition, ref int currentLine, 
                             int initialCursorLeft, int initialCursorTop)
    {
        if (_history.Count > 0)
        {
            if (_historyIndex == -1)
                _historyIndex = _history.Count;
                
            if (_historyIndex > 0)
            {
                _historyIndex--;
                SetTextFromHistory(input, _history[_historyIndex], ref cursorPosition, ref currentLine, 
                                  initialCursorLeft, initialCursorTop);
            }
        }
    }
    
    private void HandleDownArrow(StringBuilder input, ref int cursorPosition, ref int currentLine, 
                               int initialCursorLeft, int initialCursorTop)
    {
        if (_historyIndex != -1)
        {
            if (_historyIndex < _history.Count - 1)
            {
                _historyIndex++;
                SetTextFromHistory(input, _history[_historyIndex], ref cursorPosition, ref currentLine, 
                                  initialCursorLeft, initialCursorTop);
            }
            else
            {
                // Past the most recent history item - clear input
                _historyIndex = -1;
                SetTextFromHistory(input, string.Empty, ref cursorPosition, ref currentLine, 
                                  initialCursorLeft, initialCursorTop);
            }
        }
    }
    
    private void HandleHome(StringBuilder input, ref int cursorPosition, ref int currentLine, 
                          int initialCursorLeft, int initialCursorTop)
    {
        // Move to beginning of current line
        string text = input.ToString();
        int lastNewline = text.LastIndexOf(NEWLINE, Math.Max(0, cursorPosition - 1));
        
        if (lastNewline >= 0)
            cursorPosition = lastNewline + 1;
        else
            cursorPosition = 0;
            
        RedrawInput(text, cursorPosition, initialCursorLeft, initialCursorTop);
    }
    
    private void HandleEnd(StringBuilder input, ref int cursorPosition, ref int currentLine, 
                         int initialCursorLeft, int initialCursorTop)
    {
        // Move to end of current line
        string text = input.ToString();
        int nextNewline = text.IndexOf(NEWLINE, cursorPosition);
        
        if (nextNewline >= 0)
            cursorPosition = nextNewline;
        else
            cursorPosition = text.Length;
            
        RedrawInput(text, cursorPosition, initialCursorLeft, initialCursorTop);
    }
    
    private void SetTextFromHistory(StringBuilder input, string historyText, ref int cursorPosition, 
                                  ref int currentLine, int initialCursorLeft, int initialCursorTop)
    {
        input.Clear();
        input.Append(historyText);
        cursorPosition = input.Length;
        
        // Recalculate current line
        currentLine = CountLines(input.ToString()) - 1;
        
        RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
    }
    
    private void RedrawInput(string text, int cursorPosition, int initialLeft, int initialTop)
    {
        try
        {
            // Clear the input area
            Console.SetCursorPosition(initialLeft, initialTop);
            Console.Write(new string(' ', Console.WindowWidth - initialLeft));
            
            // For multiline text, clear additional lines
            int lineCount = CountLines(text);
            for (int i = 1; i < lineCount; i++)
            {
                Console.SetCursorPosition(0, initialTop + i);
                Console.Write(new string(' ', Console.WindowWidth));
            }
            
            // Display the text
            Console.SetCursorPosition(initialLeft, initialTop);
            Console.Write(text);
            
            // Position the cursor correctly
            PositionCursor(text, cursorPosition, initialLeft, initialTop);
        }
        catch (ArgumentOutOfRangeException)
        {
            // Handle console buffer issues gracefully
            Console.WriteLine();
            Console.Write(text);
        }
    }
    
    private void PositionCursor(string text, int cursorPosition, int initialLeft, int initialTop)
    {
        if (cursorPosition <= 0)
        {
            Console.SetCursorPosition(initialLeft, initialTop);
            return;
        }
        
        string textBeforeCursor = text.Substring(0, Math.Min(cursorPosition, text.Length));
        int lastNewlinePos = textBeforeCursor.LastIndexOf(NEWLINE);
        
        if (lastNewlinePos == -1)
        {
            // On first line
            Console.SetCursorPosition(initialLeft + cursorPosition, initialTop);
        }
        else
        {
            // On a line after newline
            int charsAfterNewline = cursorPosition - lastNewlinePos - 1;
            int lineNumber = textBeforeCursor.Count(c => c == '\n');
            Console.SetCursorPosition(charsAfterNewline, initialTop + lineNumber);
        }
    }
    
    private int CountLines(string text)
    {
        if (string.IsNullOrEmpty(text))
            return 1;
        
        return text.Count(c => c == '\n') + 1;
    }
}
