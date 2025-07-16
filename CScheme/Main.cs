using static CScheme.Interpreter;

var (env, _) = Load(Env, "Prelude.scm");
Console.WriteLine("Welcome to CScheme");
Repl(env);
