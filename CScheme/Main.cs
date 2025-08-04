using System.Reflection;
using System.Text;
using CScheme;
using static CScheme.Scheme;
using Environment = System.Collections.Immutable.ImmutableDictionary<string, CScheme.Expression>;

var (env, _) = Load(Env, "Prelude.scm");
env = DefineNativeFunction("now", () => DateTime.Now, env);
var httpClient = new HttpClient();
env = DefineNativeFunction<string>("http-get", uri => httpClient.GetStringAsync(uri).GetAwaiter().GetResult(), env);
env = DefineNativeFunction<string, object>("__get", (method, instance) =>
            instance.GetType()
                .InvokeMember(method, 
                    BindingFlags.GetProperty | BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase, null, instance, [])!, env);

Console.WriteLine("Welcome to CScheme");
Repl(env);

static void Repl(Environment env)
{
    var reader = new ConsoleInputReader();
    
    void InternalRepl(Environment env)
    {
        var trace = false;
        
        while (true)
        {
            Console.Write("> ");
            var line = reader.ReadLine();
            if (string.IsNullOrWhiteSpace(line)) continue;
            try
            {
                if (line == ".tests")
                {
                    (env, _) = Load(Env, "Tests.scm");
                }
                else if (line == ".trace")
                {
                    Trace = true;
                    Console.WriteLine("Trace on");
                }
                else if (line == ".notrace")
                {
                    Trace = false;
                    Console.WriteLine("Trace off");
                }
                else
                {
                    var parsingResult = Parse(Tokenizer.Tokenize(line).ToArray());
                    var (penv1, expressionResult) = Eval(env, parsingResult[0]);
                    var (penv, result) = ((Environment Env, string Result)) (penv: penv1, Print(expressionResult));
                    if (!string.IsNullOrWhiteSpace(result))
                    {
                        Console.WriteLine(result);
                    }
                    env = penv;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
            }
        }
    }

    InternalRepl(env);
}

public class ConsoleInputReader
{
    private List<string> history = [];
    private int historyIndex = -1;

    public string ReadLine()
    {
        var input = new StringBuilder();
        var cursorPosition = 0;
        var currentLine = 0;
        var lines = new List<string>{string.Empty};

        // Save current cursor position to handle multi-line input properly
        var initialCursorLeft = Console.CursorLeft;
        var initialCursorTop = Console.CursorTop;

        while (true)
        {
            var keyInfo = Console.ReadKey(true);

            if (keyInfo.Key == ConsoleKey.Enter)
            {
                if (keyInfo.Modifiers.HasFlag(ConsoleModifiers.Alt))
                {
                    // Shift+Enter - add a new line
                    lines.Insert(currentLine + 1, "");
                    input.Insert(cursorPosition, System.Environment.NewLine);
                    cursorPosition += System.Environment.NewLine.Length;
                    currentLine++;
                    
                    // Update console display
                    Console.WriteLine();
                    var text = input.ToString();
                    if (Console.CursorTop == Console.BufferHeight - 1)
                    {
                        initialCursorTop = Console.BufferHeight - currentLine - 1;
                    }
                    RedrawInput(text, cursorPosition, initialCursorLeft, initialCursorTop);
                }
                else
                {
                    // Regular Enter - submit the input
                    Console.WriteLine();
                    string result = input.ToString();
                    
                    // Add to history if not empty and not a duplicate of the last entry
                    if (!string.IsNullOrEmpty(result) && 
                        (history.Count == 0 || result != history[^1]))
                    {
                        history.Add(result);
                    }
                    historyIndex = history.Count;
                    
                    return result;
                }
            }
            else if (keyInfo.Key == ConsoleKey.Backspace && cursorPosition > 0)
            {
                // Handle backspace
                cursorPosition--;
                input.Remove(cursorPosition, 1);
                
                // Update line count if we deleted a newline
                if (cursorPosition > 0 && 
                    input.Length >= cursorPosition && 
                    input.ToString().Substring(cursorPosition - 1, 1) == "\n")
                {
                    currentLine--;
                }
                
                RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
            }
            else if (keyInfo.Key == ConsoleKey.Delete && cursorPosition < input.Length)
            {
                // Handle delete key
                input.Remove(cursorPosition, 1);
                RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
            }
            else if (keyInfo.Key == ConsoleKey.LeftArrow && cursorPosition > 0)
            {
                // Move cursor left
                cursorPosition--;
                
                // Update current line if we crossed a line boundary
                if (cursorPosition > 0 && 
                    input.ToString().Substring(cursorPosition - 1, 1) == "\n")
                {
                    currentLine--;
                }
                
                RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
            }
            else if (keyInfo.Key == ConsoleKey.RightArrow && cursorPosition < input.Length)
            {
                // Move cursor right
                cursorPosition++;
                
                // Update current line if we crossed a line boundary
                if (cursorPosition < input.Length && 
                    input.ToString().Substring(cursorPosition - 1, 1) == "\n")
                {
                    currentLine++;
                }
                
                RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
            }
            else if (keyInfo.Key == ConsoleKey.UpArrow)
            {
                // Navigate history upward
                if (history.Count > 0)
                {
                    if (historyIndex == -1)
                    {
                        // First time pressing up - save current input
                        historyIndex = history.Count;
                    }
                    
                    if (historyIndex > 0)
                    {
                        historyIndex--;
                        input.Clear();
                        input.Append(history[historyIndex]);
                        cursorPosition = input.Length;
                        
                        // Recalculate current line
                        currentLine = CountLines(input.ToString()) - 1;
                        
                        RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
                    }
                }
            }
            else if (keyInfo.Key == ConsoleKey.DownArrow)
            {
                // Navigate history downward
                if (historyIndex != -1)
                {
                    if (historyIndex < history.Count - 1)
                    {
                        historyIndex++;
                        input.Clear();
                        input.Append(history[historyIndex]);
                    }
                    else
                    {
                        // Past the most recent history item - clear input
                        historyIndex = -1;
                        input.Clear();
                    }
                    
                    cursorPosition = input.Length;
                    
                    // Recalculate current line
                    currentLine = CountLines(input.ToString()) - 1;
                    
                    RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
                }
            }
            else if (!char.IsControl(keyInfo.KeyChar))
            {
                // Regular character input
                input.Insert(cursorPosition, keyInfo.KeyChar);
                cursorPosition++;
                RedrawInput(input.ToString(), cursorPosition, initialCursorLeft, initialCursorTop);
            }
        }
    }
    
    private void RedrawInput(string text, int cursorPosition, int initialLeft, int initialTop)
    {
        // Clear the input area
        Console.SetCursorPosition(initialLeft, initialTop);
        Console.Write(new string(' ', Console.WindowWidth - initialLeft));
        
        // For multiline text, clear additional lines
        int lines = CountLines(text);
        for (int i = 1; i < lines; i++)
        {
            Console.SetCursorPosition(0, initialTop + i);
            Console.Write(new string(' ', Console.WindowWidth));
        }
        
        // Display the text
        Console.SetCursorPosition(initialLeft, initialTop);
        Console.Write(text);
        
        // Position the cursor correctly
        int currentLinePos = text.Substring(0, cursorPosition).LastIndexOf('\n');
        if (currentLinePos == -1)
        {
            // First line
            Console.SetCursorPosition(initialLeft + cursorPosition, initialTop);
        }
        else
        {
            // Line after a newline
            int linePosition = cursorPosition - currentLinePos - 1;
            int lineNumber = CountLines(text.Substring(0, cursorPosition)) - 1;
            Console.SetCursorPosition(linePosition, initialTop + lineNumber);
        }
    }
    
    private int CountLines(string text)
    {
        if (string.IsNullOrEmpty(text))
            return 1;
        
        return text.Split('\n').Length;
    }
}