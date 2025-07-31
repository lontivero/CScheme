using System.Collections.Immutable;
using System.Diagnostics;
using CScheme;
using static CScheme.Tokenizer;

using Environment = System.Collections.Immutable.ImmutableDictionary<string, CScheme.Expression>;
[assembly: DebuggerDisplay("{Interpreter.Print(this),ng}", Target = typeof(Expression))]

namespace CScheme;

public abstract record Expression;

public record EvalContext(Environment Environment, Expression Expression);


public class Interpreter
{
    private static Expression Lookup(string symbol, Environment env) =>
        env.TryGetValue(symbol, out var value)
            ? value
            : throw new InvalidOperationException($"No binding for '{symbol}'.");

    private static Environment ExtendEnvironment(IEnumerable<(string s, Expression e)> bindings, Environment env) =>
        env.SetItems(bindings.Select(x => KeyValuePair.Create(x.s, x.e)));

    private delegate Expression ExpressionsProcessor(Expression args);

    private delegate EvalContext SpecialExpressionsProcessor(Environment env, Expression args);

    private record Number(long Value) : Expression;

    private record String(string Value) : Expression;

    private record Boolean(bool Bool) : Expression;

    private record Symbol(string Value) : Expression;

    private abstract record List : Expression;

    private record Pair(Expression Car, Expression Cdr) : List;

    private record DottedList(Expression Car, Expression Cdr) : Pair(Car, Cdr);

    private record Nil : List;

    private record Function(string Name, ExpressionsProcessor Fn) : Expression;

    private record Special(string Name, SpecialExpressionsProcessor Fn) : Expression;

    private record NativeObject(object Value) : Expression;

    private record DummyExpression(string Val) : Expression;

    private static readonly Symbol QuoteExpr = new("quote");
    private static readonly Symbol QuasiQuoteExpr = new("quasiquote");
    private static readonly Symbol UnquoteExpr = new("unquote");
    private static readonly Symbol UnquoteSplicingExpr = new("unquote-splicing");
    private static readonly Boolean True = new(true);
    private static readonly Boolean False = new(false);
    private static readonly Nil NilExpr = new();
    private static readonly DummyExpression DummyExpr = new("dummy for letrec");
    private static Pair Cons(Expression Car, Expression Cdr) => new(Car, Cdr);

    private static Expression MapTokenToExpression(Token token) =>
        token switch
        {
            NumberToken n => new Number(long.Parse(n.number)),
            StringToken s => new String(s.str),
            BooleanToken s => s.b ? True : False,
            SymbolToken t => new Symbol(t.symbol),
            _ => throw new SyntaxException($"token '{token}' is not a mappeable to an expression.")
        };

    public static ImmutableArray<Expression> Parse(Token[] tokens)
    {
        var expressions = new List<Expression>();
        while (tokens.Length > 0)
        {
            var x = ParseExpression(tokens);
            expressions.Add(x.ParsedExpression);
            tokens = x.UnparsedTokens;
        }

        return [..expressions];

        static (Expression, Token[] rest) ParseList(Token[] tokens)
        {
            (Expression Pair, Token[] UnparsedTokens) ParseListElements(Expression acc, Token[] tokens) =>
                tokens switch
                {
                    [CloseToken, .. var t] => (acc, t),
                    _ => ParseExpression(tokens)
                        .AndThen(r => (Car: r.ParsedExpression, Cdr: ParseListElements(acc, r.UnparsedTokens)))
                        .AndThen(p => (Cons(p.Car, p.Cdr.Pair), p.Cdr.UnparsedTokens))
                };

            return ParseListElements(NilExpr, tokens);
        }

        static (Expression ParsedExpression, Token[] UnparsedTokens) ParseExpression(Token[] tokens) =>
            tokens switch
            {
                [OpenToken, .. var t] => ParseList(t),
                [QuoteToken, .. var t] => ParseExpression(t).AndThen(r =>
                    (Cons(QuoteExpr, r.ParsedExpression), rest: r.UnparsedTokens)),
                [QuasiQuoteToken, .. var t] => ParseExpression(t).AndThen(r =>
                    (Cons(QuasiQuoteExpr, r.ParsedExpression), rest: r.UnparsedTokens)),
                [UnquoteToken, .. var t] => ParseExpression(t).AndThen(r =>
                    (Cons(UnquoteExpr, r.ParsedExpression), rest: r.UnparsedTokens)),
                [UnquoteSplicingToken, .. var t] => ParseExpression(t).AndThen(r =>
                    (Cons(UnquoteSplicingExpr, r.ParsedExpression), rest: r.UnparsedTokens)),
                [var h, .. var t] => (MapTokenToExpression(h), t),
            };
    }

    public static EvalContext Eval(Environment env, Expression expr)
    {
        var r = expr switch
        {
            Symbol sym => new EvalContext(env, Lookup(sym.Value, env)),
            Pair(Car: var h, Cdr: var t) => Eval(env, h).AndThen(ctx => ctx.Expression switch
            {
                Function f => Apply(ctx.Environment, f.Fn, t),
                Special f => f.Fn(ctx.Environment, t),
                _ => throw SyntaxError("The first element in a list must be a function", ctx.Expression)
            }),
            var e => new EvalContext(env, e)
        };
        //Console.WriteLine($"{Print(expr)}    => {Print(r.Expression)}");
        return r;
    }

    private static EvalContext Evaluate(Environment env, Expression exprs) =>
        Eval(env, Car(exprs)).AndThen(ctx =>
            Eval(ctx.Environment, ctx.Expression));

    private static EvalContext Apply(Environment env, ExpressionsProcessor function, Expression args) =>
        Map(e => Eval(env, e).Expression, args)
            .AndThen(ars => new EvalContext(env, function(
                ars is Pair { Car: var car, Cdr: Nil} ? car : ars)));

    public static string Print(Expression expr) =>
        expr switch
        {
            Nil => "'()",
            List lst => $"({PrintList(lst)})",
            String str => str.Value,
            Symbol sym => sym.Value,
            Number num => num.Value.ToString(),
            Boolean b => b.Bool ? "#t" : "#f",
            Function f => f.Name,
            Special s => s.Name,
            NativeObject o => $"{o.Value}",
            DummyExpression => string.Empty,
            _ => throw new ArgumentOutOfRangeException(nameof(expr))
        };

    private static string PrintList(Expression lst) =>
        lst switch
        {
            Pair {Car: var car, Cdr: Nil} => $"{Print(car)}",
            Pair {Car: var car, Cdr: var cdr and not Pair} => $"{PrintList(car)} . {PrintList(cdr)}",
            Pair {Car: var car, Cdr: var cdr} => Print(car) + " " + PrintList(cdr),
            _ => Print(lst)
        };

private static ExpressionsProcessor Math(long identity, long unary, Func<long, long, long> op) =>
        es => es switch
        {
            Nil => new Number(identity),
            Number n => new Number(unary * n.Value),
            Pair {Car: Number { Value: var n }, Cdr: var ns} => new Number(Fold(n, (acc, e) => op(acc, e is Number { Value: var nn } ? nn : throw SyntaxError("ss", e)), ns)), 
            _ => throw SyntaxError("Math can only involve number", es)
        };

    private static ExpressionsProcessor Compare(Func<long, long, bool> op) =>
        es => es is Pair {Car: Number a, Cdr: Pair { Car: Number b} }
            ? op(a.Value, b.Value) ? True : False
            : throw SyntaxError("Binary comparison requires two expressions", es);

    private static ExpressionsProcessor NumericEquality =>
        es => es is Pair {Car: Number a, Cdr: Pair { Car: Number b} }
            ? a.Value == b.Value ? True : False
            : throw SyntaxError("Binary comparison requires two expressions", es);

    private static ExpressionsProcessor IdentityEquality =>
        es => es switch
        {
            Pair { Car: Symbol a, Cdr: Pair { Car: Symbol b } } => a.Value == b.Value ? True : False,
            Pair { Car: Number a, Cdr: Pair { Car: Number b } } => a.Value == b.Value ? True : False,
            Pair { Car: String a, Cdr: Pair { Car: String b } } =>  a.Value == b.Value ? True : False,
            Pair { Car: Boolean a, Cdr: Pair { Car: Boolean b } } => a.Bool == b.Bool ? True : False,
            Pair { Car: Pair a, Cdr: Pair { Car: Pair b } } => ReferenceEquals(a, b) ? True : False,
            Pair { Car: Function a, Cdr: Pair { Car: Function b } } => ReferenceEquals(a.Fn, b.Fn) ? True : False,
            Pair { Car: Special a, Cdr: Pair { Car: Special b } } => ReferenceEquals(a.Fn, b.Fn) ? True : False,
            Pair { Car: Nil, Cdr: Pair { Car: Nil } } => True,
            _ => throw SyntaxError("Binary comparison requires two expressions", es)
        };
    
    private static ExpressionsProcessor CompareStructuralEquality =>
        es => es switch
        {
            Pair { Car: Number a, Cdr: Pair { Car: Number b}} => a.Value == b.Value ? True : False,
            Pair { Car: String a, Cdr: Pair { Car: String b} }=> a.Value == b.Value ? True : False,
            Pair { Car: Boolean a, Cdr: Pair { Car: Boolean b}} => a.Bool == b.Bool ? True : False,
            Pair { Car: Symbol a, Cdr: Pair { Car: Symbol b}} => a.Value == b.Value ? True : False,
            Pair { Car: Function a, Cdr: Pair { Car: Function b}} => a.Fn == b.Fn ? True : False,
            Pair { Car: Special a, Cdr: Pair { Car: Special b}} => a.Fn == b.Fn ? True : False,
            Pair { Car: Pair a, Cdr: Pair { Car: Pair b}} => a == b ? True : False,
            Pair { Car: Nil, Cdr: Pair { Car: Nil}} => True,
            _ => False
        };
    
    private static readonly ExpressionsProcessor Add = Math(0L, 1L, (a, b) => a + b);
    private static readonly ExpressionsProcessor Subtract = Math(0L, -1L, (a, b) => a - b);
    private static readonly ExpressionsProcessor Multiply = Math(1L, 1L, (a, b) => a * b);
    private static readonly ExpressionsProcessor Divide = Math(1L, 1L, (a, b) => a / b);
    private static readonly ExpressionsProcessor Modulus = Math(1L, 1L, (a, b) => a % b);

    private static readonly ExpressionsProcessor Greater = Compare((a, b) => a > b);
    private static readonly ExpressionsProcessor Less = Compare((a, b) => a < b);

    private static Expression Car(Expression es) =>
        es is Pair { Car: var car } ? car : throw SyntaxError("'car'", es);

    private static Expression Cdr(Expression es) =>
        es is Pair { Cdr: var cdr } ? cdr : throw SyntaxError("'cdr'", es);

    private static Expression Cons(Expression es) =>
        es is Pair {Car: var car, Cdr: Pair {Car: var cdr, Cdr: Nil}}
            ? new Pair(car, cdr)
            : throw SyntaxError("'cons'", es);

    private static EvalContext If(Environment env, Expression exprs) =>
        exprs is Pair {Car: var condition, Cdr: Pair {Car: var t, Cdr: var f}}
            ? Eval(env, condition).AndThen(ctx => ctx.Expression switch
            {
                Boolean {Bool: false} => Eval(ctx.Environment, Car(f)),
                _ => Eval(ctx.Environment, t) // everything else is true
            })
            : throw SyntaxError("'if' must have a condition and true expressions", exprs);

    private static EvalContext Let(Environment env, Expression exprs)
    {
        if (exprs is not Pair {Car: var bindings, Cdr: var body})
            throw SyntaxError("'let' must have bindings and a body expression.", exprs);
        
        return Eval(Fold(env, Bind, bindings), WrapBegin(body)) with{ Environment = env };

        Environment Bind(Environment penv, Expression binding) =>
            binding is Pair {Car: Symbol {Value: var sym}, Cdr: var e}
                ? ExtendEnvironment([(sym, Eval(env, Car(e)).Expression)], penv)
                : throw SyntaxError("'let' binding.", binding);
    }

    private static EvalContext LetRec(Environment env, Expression exprs)
    {
        if (exprs is not Pair { Car: var bindings, Cdr: var body })
            throw SyntaxError("'let' must have bindings and a body expression.", exprs);

        return Eval(Fold(env, Bind, bindings), WrapBegin(body)) with{ Environment = env };

        Environment Bind(Environment ppenv, Expression binding) =>
            binding is Pair {Car: Symbol {Value: var sym}, Cdr: var e}
                ? Eval(ppenv, Car(e)).AndThen(r => ExtendEnvironment([(sym, r.Expression)], r.Environment))
                : throw SyntaxError("'let' binding.", binding);
    }

    private static EvalContext LetStar(Environment env, Expression exprs)
    {
        if (exprs is not Pair {Car: var bindings, Cdr: var body})
            throw SyntaxError("'let*' must have bindings and a body expression.", exprs);

        return Eval(Fold(env, Bind, bindings), WrapBegin(body))with{ Environment = env };
        
        Environment Bind(Environment penv, Expression bind) =>
            bind is Pair {Car: Symbol {Value: var sym}, Cdr: var e}
                ? ExtendEnvironment([(sym, Eval(penv, Car(e)).Expression)], penv)
                : throw SyntaxError("'let' binding.", bind);
    }

    private static EvalContext Lambda(Environment env, Expression expr)
    {
        if (expr is not Pair {Car: var parameters, Cdr: var body})
            throw SyntaxError("'lambda'", expr);

        return new EvalContext(env, new Special("anonymous", Closure));

        EvalContext Closure(Environment callerEnv, Expression args)
        {
            var penv = env.SetItems(callerEnv);
            var bindings = Zip(parameters, args);
            return Eval(Fold(penv, Bind, bindings), WrapBegin(body));

            Environment Bind(Environment penv, Expression binding) =>
                binding switch
                {
                    DottedList {Car: Symbol {Value: var sym}, Cdr: List lst} =>
                        Map(e => Eval(callerEnv, e).Expression, lst)
                            .AndThen(r => ExtendEnvironment([(sym, r)], penv)),
                    Pair {Car: Symbol {Value: var sym}, Cdr: var e} =>
                        ExtendEnvironment([(sym, Eval(callerEnv, e).Expression)], penv),
                    _ => throw SyntaxError("'lambda' binding.", binding)
                };
        }
    }

    private static Expression WrapBegin(Expression exprs) =>
        exprs switch
        {
            Pair { Car: var car, Cdr: Nil } => car,
            Pair { Car: Pair } p => Cons(new Symbol("begin"), p),
            var e => e
        };


    private static EvalContext QuasiQuote(Environment env, Expression exprs)
    {
        var u = Unquote(exprs);
        return new EvalContext(env, u);

        Expression Unquote(Expression expr) =>
            expr switch
            {
                Pair p => UnquoteList(p),
                var e => e
            };

        Expression UnquoteList(Expression expr) =>
            expr switch
            {
                Pair { Car: Symbol("unquote"), Cdr: var cdr } => 
                    Eval(env, cdr).Expression,
                
                Pair { Car: var car, Cdr: Pair { Car: Pair { Car: Symbol("unquote-splicing"), Cdr: Pair { Car: var cdr} } } } =>
                    Cons(car, Eval(env, cdr).Expression),
                Pair { Car: var car, Cdr: Pair { Car: Pair { Car: Symbol("unquote-splicing"), Cdr: var cdr } } } =>
                    Cons(car, Eval(env, cdr).Expression),

                Pair {Car: var car, Cdr: var cdr} =>
                    Cons(Unquote(car), UnquoteList(cdr)),
                Nil => NilExpr,
                var e => Cons(e, NilExpr)
            };
    }

    private static EvalContext Quote(Environment env, Expression exprs) =>
        new (env, exprs);

    private static EvalContext Begin(Environment env, Expression exprs) =>
        Fold(new EvalContext(env, NilExpr), (ctx, e) => Eval(ctx.Environment, e), exprs);

    private static EvalContext Define(Environment env, Expression exprs)
    {
        return exprs switch
        {
            Pair { Car: Symbol {Value: var sym}, Cdr: Pair { Car: var e }} => InternalDefine(sym, e),
            Pair { Car: Pair { Car: Symbol { Value: var sym }, Cdr: var ps }, Cdr: var body } =>
                InternalDefine(sym, Cons(new Symbol("lambda"), Cons(ps, WrapBegin(body)))),
            _ => throw SyntaxError("'Define'", exprs)
        };

        EvalContext InternalDefine(string sym, Expression e)
        {
            var (penv, pe) = Eval(env, e);
            var ppenv = ExtendEnvironment([(sym, pe)], penv);
            return new EvalContext(ppenv, new DummyExpression($"Define {sym}"));
        }
    }

    private static EvalContext DefineMacro(Environment env, Expression exprs)
    {
        if (exprs is not Pair { Car: Pair{ Car: Symbol {Value: var sym }, Cdr: var parameters}, Cdr: Pair { Car: var body }})
        {
            throw SyntaxError("'define-macro'", exprs);
        }

        var penv = ExtendEnvironment([(sym, new Special("anonymous", Closure))], env);
        return new EvalContext(penv, new DummyExpression($"Define Macro {sym}"));
        
        EvalContext Closure(Environment callerEnv, Expression args)
        {
            var binding = Zip(parameters, args);
            return Eval(Fold(callerEnv, Bind, binding), body)
                .AndThen(ctx => Eval(ctx.Environment, ctx.Expression));

            Environment Bind(Environment penv, Expression pexpr) =>
                pexpr is Pair {Car: Symbol {Value: var s}, Cdr: var e}
                    ? ExtendEnvironment([(s, e)], penv)
                    : throw SyntaxError("'macro' parameter.", pexpr);
        }
    }

    private static EvalContext Set(Environment env, Expression exprs) =>
        exprs is Pair { Car: Symbol {Value: var sym}, Cdr: var e } 
            ? Eval(env, e).AndThen(ctx =>
                // set! is dangerous because it alters the bindings table. Should I remove it???
                new EvalContext(env.SetItem(sym, ctx.Expression), new DummyExpression($"Set {sym}")))
            : throw SyntaxError("set!", exprs);

    private static Expression NullQm(Expression es) =>
        es is Nil or Pair { Car: Nil, Cdr: Nil } ? True : False;
    
    private static Expression Display(Expression e)
    {
        Console.Write(Print(e));
        return new DummyExpression("Dummy 'display'");
    }

    private static EvalContext Load(Environment env, Expression exprs)
    {
        if (exprs is not Pair { Car: String {Value: var filename} })
        {
            throw SyntaxError("'load'", exprs);
        }

        // var ts = Tokenize("(define nil '())");
        var tokens = Tokenize(File.OpenText(filename).ReadToEnd());
        var parsingResults = Parse(tokens.ToArray());
        foreach (var result in parsingResults)
        {
            (env, _) = Eval(env, result);
        }

        return new EvalContext(env, new Symbol($"Loaded '{filename}'"));
    }

    public static EvalContext Load(Environment env, string filename) =>
        Load(env, Cons(new String(filename), NilExpr));

    private static Boolean Is<T>(Expression xs) => xs is T ? True : False;
    private static Boolean IsList(Expression xs)
    {
        static bool InternalIsList(Expression e) => e is Nil || e is Pair { Cdr: var cdr } && InternalIsList(cdr);
        return InternalIsList(xs) ? True : False;
    }

    public static Environment DefineNativeFunction(string fname, Func<object> fn, Environment env) =>
        InternalDefineNativeFunction(fname, os => fn(), env);
    
    public static Environment DefineNativeFunction<T>(string fname, Func<T, object> fn, Environment env) =>
        InternalDefineNativeFunction(fname, os => fn((T)os[0]), env);
    
    public static Environment DefineNativeFunction<T0, T1>(string fname, Func<T0, T1, object> fn, Environment env) =>
        InternalDefineNativeFunction(fname, os => fn((T0)os[0], (T1)os[1]), env);
    
    private static Environment InternalDefineNativeFunction(string fname, Func<object[], object> fn, Environment env)
    {
        Expression ConvertNativeToScheme(object obj) =>
            obj switch
            {
                int intValue => new Number(intValue),
                string stringValue => new String(stringValue),
                bool booleanValue => booleanValue ? True : False,
                IEnumerable<object> e => e.Select(ConvertNativeToScheme).Reverse().Aggregate((Expression)NilExpr, (acc, expr) => Cons(expr, acc)),
                var o => new NativeObject(o)
            };
        
        object ConvertSchemeToNative(Expression e) =>
            e switch
            {
                Number { Value: var intValue } => intValue,
                String { Value: var stringValue } => stringValue,
                Boolean { Bool: var boolValue } => boolValue,
                NativeObject { Value: var objValue } => objValue,
                Symbol { Value: var symValue } => symValue,
                Pair pairValue => FlatPairChain(pairValue).Select(ConvertSchemeToNative)
            };
            
        Expression WrapNativeFunction(Expression exprs)
        {
            var parameters = ConvertSchemeToNative(exprs);
            var result = fn.Invoke(parameters is IEnumerable<object> enumerable ? enumerable.ToArray() : [ parameters ]);
            var nativeResult = ConvertNativeToScheme(result);
            return nativeResult;
        }
        var fnative = new Function(fname, WrapNativeFunction);
        
        return env.Add(fname, fnative);
    }

    private static ImmutableArray<Expression> FlatPairChain(List p) =>
        Fold(ImmutableArray<Expression>.Empty, (acc, e) => acc.Add(e), p);
    
    public static readonly Environment Env = new Dictionary<string, Expression> {
        { "*", new Function("*",Multiply) },
        { "/", new Function("/",Divide) },
        { "%", new Function("%",Modulus) },
        { "+", new Function("+",Add) },
        { "-", new Function("-",Subtract) },
        { "=", new Function("=",NumericEquality) },
        { "eq?", new Function("eq?",IdentityEquality) },
        { "equal?", new Function("equal?",CompareStructuralEquality) },
        { ">", new Function(">",Greater) },
        { "<", new Function("<",Less) },
        { "null?", new Function("null?",NullQm) },
        { "if", new Special("if",If) },
        { "let", new Special("let",Let) },
        { "letrec", new Special("letrec",LetRec) },
        { "let*", new Special("let*",LetStar) },
        { "lambda", new Special("lambda",Lambda) },
        { "cons", new Function("cons",Cons) },
        { "car", new Function("car",Car) },
        { "cdr", new Function("cdr",Cdr) },
        { "quote", new Special("quote",Quote) },
        { "quasiquote", new Special("quasiquote",QuasiQuote) },
        { "unquote", new Function("unquote",e => DummyExpr)},
        { "eval", new Special("eval",Evaluate) },
        { "define-macro", new Special("define-macro",DefineMacro) },
        { "set!", new Special("set!",Set) },
        { "begin", new Special("begin",Begin) },
        { "define", new Special("define",Define) },
        { "load", new Special("load",Load) },
        { "display", new Function("display",Display) },
        { "number?", new Function("number?",Is<Number>) },
        { "string?", new Function("string?",Is<String>) },
        { "boolean?", new Function("boolean?",Is<Boolean>) },
        { "symbol?", new Function("symbol?",Is<Symbol>) },
        { "list?", new Function("list?",IsList) },
        { "pair?", new Function("pair?",Is<Pair>) },
        { "not", new Function("not",e => e is Boolean { Bool: false } ? True : False)}
    }.ToImmutableDictionary();

    private static List Zip(Expression ps,
        Expression args)
    {
        return ZipDotted(NilExpr, ps, args);

        List ZipDotted(List acc, Expression pps, Expression pas) =>
            (pps, pas) switch
            {
                (Symbol p, var tas) => Cons(Cons(p, tas), acc),
                (Pair { Car: Symbol("."), Cdr: Pair { Car: Symbol p} }, var tas) => Cons(new DottedList(p, tas), acc),
                (Pair { Car: Symbol p}, Pair { Car: var a}) => Cons(Cons(p, a), ZipDotted(acc, Cdr(pps), Cdr(pas))),
                (Nil,Nil) => acc,
                _ => throw SyntaxError($"parameters were expected but were passed.", ps)
                //_ => throw SyntaxError($"{ps.Length} parameters were expected but {args.Length} were passed.", ps)
            };
    }

    private static Expression Map(ExpressionsProcessor fn, Expression s) =>
        s switch
        {
            Nil => NilExpr,
            Pair {Car: var h, Cdr: var t} => Cons(fn(h), Map(fn, t)),
            var o => fn(o)
        };
    
    private static T Fold<T>(T acc, Func<T, Expression, T> fn, Expression p) =>
        p switch
        {
            Nil => acc,
            Pair { Car: var car, Cdr: var cdr } => Fold(fn(acc, car), fn, cdr),
            var e => fn(acc, e)
        };
    
    private static T Foldr<T>(Func<Expression, T, T> fn, T acc, Expression p) =>
        p switch
        {
            Nil => acc,
            Pair { Car: var car, Cdr: var cdr } => fn(car, Foldr(fn, acc, cdr)),
            var e => fn(e, acc),
            //_ => throw SyntaxError("Foldr", p)
        };
    
    private static SyntaxException SyntaxError(string msg, Expression e) =>
        new($"[Syntax error] {msg} {Print(e)}");
    
    public class SyntaxException(string msg) : Exception(msg);
}

[DebuggerStepThrough]
public static class FunctionExtensions
{
    public static T AndThen<R,T>(this R me, Func<R, T> then) => then(me);
}
