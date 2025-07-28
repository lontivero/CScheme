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

    private delegate Expression ExpressionsProcessor(ImmutableArray<Expression> args);

    private delegate EvalContext SpecialExpressionsProcessor(Environment env, ImmutableArray<Expression> args);

    private record Number(long Value) : Expression;

    private record String(string Value) : Expression;
    private record Boolean(bool Bool) : Expression;

    private record Symbol(string Value) : Expression;

    private record List(ImmutableArray<Expression> Expressions) : Expression;

    private record Function(ExpressionsProcessor Fn) : Expression;

    private record Special(SpecialExpressionsProcessor Fn) : Expression;

    private record NativeObject(object Value) : Expression;

    private record DummyExpression(string Val) : Expression;

    private static readonly Symbol QuoteExpr = new("quote");
    private static readonly Symbol QuasiQuoteExpr = new("quasiquote");
    private static readonly Symbol UnquoteExpr = new("unquote");
    private static readonly Symbol UnquoteSplicingExpr = new("unquote-splicing");
    private static readonly Boolean True = new(true);
    private static readonly Boolean False = new(false);
    private static readonly DummyExpression DummyExpr = new("dummy for letrec");
    
    private static List ListExpr(ImmutableArray<Expression> exprs) => new(exprs);

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
            (ImmutableArray<Expression>, Token[]) ParseListElements(ImmutableArray<Expression> acc, Token[] tokens) =>
                tokens switch
                {
                    [CloseToken, .. var t] => (acc, t),
                    _ => ParseExpression(tokens)
                        .AndThen(r => (acc.Add(r.ParsedExpression), rest: r.UnparsedTokens))
                        .AndThen(r => ParseListElements(r.Item1, r.rest))
                };

            var (elements, unparsedTokens) = ParseListElements([], tokens);
            return (ListExpr(elements), unparsedTokens);
        }

        static (Expression ParsedExpression, Token[] UnparsedTokens) ParseExpression(Token[] tokens) =>
            tokens switch
            {
                [OpenToken, .. var t] => ParseList(t),
                [QuoteToken, .. var t] => ParseExpression(t).AndThen(r =>
                    (ListExpr([QuoteExpr, r.ParsedExpression]), rest: r.UnparsedTokens)),
                [QuasiQuoteToken, .. var t] => ParseExpression(t).AndThen(r =>
                    (ListExpr([QuasiQuoteExpr, r.ParsedExpression]), rest: r.UnparsedTokens)),
                [UnquoteToken, .. var t] => ParseExpression(t).AndThen(r =>
                    (ListExpr([UnquoteExpr, r.ParsedExpression]), rest: r.UnparsedTokens)),
                [UnquoteSplicingToken, .. var t] => ParseExpression(t).AndThen(r =>
                    (ListExpr([UnquoteSplicingExpr, r.ParsedExpression]), rest: r.UnparsedTokens)),
                [var h, .. var t] => (MapTokenToExpression(h), t),
            };
    }
    

    public static EvalContext Eval(Environment env, Expression expr) =>
        expr switch
        {
            Symbol sym => new EvalContext(env, Lookup(sym.Value, env)),
            List {Expressions: [var h, .. var t]} => Eval(env, h).AndThen(ctx => ctx.Expression switch
            {
                Function f => Apply(ctx.Environment, f.Fn, t),
                Special f => f.Fn(ctx.Environment, t),
                _ => throw SyntaxError("The first element in a list must be a function", [expr, ctx.Expression])
            }),
            var e => new EvalContext(env, e)
        };

    private static EvalContext Apply(Environment env, ExpressionsProcessor function, ImmutableArray<Expression> args) =>
        Fold(ImmutableArray<Expression>.Empty, (acc, e) => acc.Add(Eval(env, e).Expression), args)
            .AndThen(ars => new EvalContext(env, function(ars)));

    public static string Print(Expression expr) =>
        expr switch
        {
            List lst => $"({string.Join(" ", lst.Expressions.Select(Print))})",
            String str => str.Value,
            Symbol sym => sym.Value,
            Number num => num.Value.ToString(),
            Boolean b => b.Bool ? "#t" : "#f",
            Function or Special => "Function",
            NativeObject o => $"{o.Value}",
            DummyExpression => string.Empty,
            _ => throw new ArgumentOutOfRangeException(nameof(expr))
        };

    private static ExpressionsProcessor Math(long identity, long unary, Func<long, long, long> op) =>
        es => es switch
        {
            [] => new Number(identity),
            [Number n] => new Number(unary * n.Value),
            [Number n, .. var ns] => new Number(ns.Cast<Number>().Select(x => x.Value)
                .Aggregate(n.Value, op)),
            _ => throw SyntaxError("Math can only involve number", es)
        };

    private static ExpressionsProcessor Compare(Func<long, long, bool> op) =>
        es => es is [Number a, Number b]
            ? op(a.Value, b.Value) ? True : False
            : throw SyntaxError("Binary comparison requires two expressions", es);

    private static ExpressionsProcessor NumericEquality =>
        es => es is [Number a, Number b] 
            ? a.Value == b.Value ? True : False
            : throw SyntaxError("Binary comparison requires two expressions", es);

    private static ExpressionsProcessor IdentityEquality =>
        es => es switch
        {
            [Symbol a, Symbol b] => a.Value == b.Value ? True : False,
            [Number a, Number b] => a.Value == b.Value ? True : False,
            [String a, String b] =>  a.Value == b.Value ? True : False,
            [Boolean a, Boolean b] => a.Bool == b.Bool ? True : False,
            [List a, List b] => a.Expressions == b.Expressions ? True : False,
            [Function a, Function b] => ReferenceEquals(a.Fn, b.Fn) ? True : False,
            [Special a, Special b] => ReferenceEquals(a.Fn, b.Fn) ? True : False,
            _ => throw SyntaxError("Binary comparison requires two expressions", es)
        };
    
    private static ExpressionsProcessor CompareStructuralEquality =>
        es => es switch
        {
            [Number a, Number b] => a.Value == b.Value ? True : False,
            [String a, String b] => a.Value == b.Value ? True : False,
            [Boolean a, Boolean b] => a.Bool == b.Bool ? True : False,
            [Symbol a, Symbol b] => a.Value == b.Value ? True : False,
            [Function a, Function b] => a.Fn == b.Fn ? True : False,
            [Special a, Special b] => a.Fn == b.Fn ? True : False,
            [List a, List b] when a.Expressions.Length == b.Expressions.Length => 
                a.Expressions
                    .Zip(b.Expressions, (pa, pb)=>(pa, pb))
                    .All(x => CompareStructuralEquality([x.pa, x.pb]) is Boolean {Bool: true})
                    ? True : False,
            _ => False
        };
    
    private static readonly ExpressionsProcessor Add = Math(0L, 1L, (a, b) => a + b);
    private static readonly ExpressionsProcessor Subtract = Math(0L, -1L, (a, b) => a - b);
    private static readonly ExpressionsProcessor Multiply = Math(1L, 1L, (a, b) => a * b);
    private static readonly ExpressionsProcessor Divide = Math(1L, 1L, (a, b) => a / b);
    private static readonly ExpressionsProcessor Modulus = Math(1L, 1L, (a, b) => a % b);

    private static readonly ExpressionsProcessor Greater = Compare((a, b) => a > b);
    private static readonly ExpressionsProcessor Less = Compare((a, b) => a < b);

    private static Expression Car(ImmutableArray<Expression> es) =>
        es is [List {Expressions: [var e, .. _]}] ? e : throw SyntaxError("'car'", es);

    private static Expression Cdr(ImmutableArray<Expression> es) =>
        es is [List {Expressions: [_, .. var t]}] ? ListExpr(t) : throw SyntaxError("'cdr'", es);

    private static Expression Cons(ImmutableArray<Expression> es) =>
        es switch
        {
            [{ } es1, List {Expressions: var es2}] => ListExpr(es2.Insert(0, es1)),
            [{ } es1, var es2] => ListExpr([es1, es2]),
            _ => throw SyntaxError("'list'", es)
        };

    private static EvalContext If(Environment env, ImmutableArray<Expression> exprs) =>
        exprs switch
        {
            [var condition, var t, var f] => Eval(env, condition).AndThen(ctx => ctx.Expression switch
            {
                Boolean {Bool: false} => Eval(ctx.Environment, f), 
                _ => Eval(ctx.Environment, t) // everything else is true
            }),
            _ => throw SyntaxError("'if' must have a condition and true expressions", exprs)
        };

    private static EvalContext Let(Environment env, ImmutableArray<Expression> exprs)
    {
        if(exprs is not [List {Expressions: var bindings}, .. var body])
        {
            throw SyntaxError("'let' must have bindings and a body expression.", exprs);
        }

        return Eval(Fold(env, Bind, bindings), WrapBegin(body)) with{ Environment = env };

        Environment Bind(Environment penv, Expression pexpr) =>
            pexpr is List { Expressions: [Symbol {Value: var s}, var e] }
                ? Eval(env, e).AndThen(r => ExtendEnvironment([(s, r.Expression)], penv))
                : throw SyntaxError("'let' binding.", bindings);
    }

    private static EvalContext LetRec(Environment env, ImmutableArray<Expression> exprs)
    {
        if (exprs is not [List {Expressions: var bindings}, .. var body])
        {
            throw SyntaxError("'let' must have bindings and a body expression.", exprs);
        }

        return Eval(Fold(env, Bind, bindings), WrapBegin(body)) with{ Environment = env };

        Environment Bind(Environment penv, Expression pexpr) =>
            pexpr is List { Expressions: [ Symbol {Value: var sym}, var e ] }
                ? Eval(penv, e).AndThen(r => ExtendEnvironment([(sym, r.Expression)], r.Environment))
                : throw SyntaxError("'let' binding.", [pexpr]);
    }

    private static EvalContext LetStar(Environment env, ImmutableArray<Expression> exprs)
    {
        if (exprs is not [List {Expressions: var bindings}, .. var body])
        {
            throw SyntaxError("'let*' must have bindings and a body expression.", exprs);
        }

        return Eval(Fold(env, Bind, bindings), WrapBegin(body)) with{ Environment = env };
        
        Environment Bind(Environment penv, Expression bind) =>
            bind is List {Expressions: [Symbol {Value: var sym}, var e]}
                ? ExtendEnvironment([(sym, Eval(penv, e).Expression)], penv)
                : throw SyntaxError("'let*' binding.", [bind]);
    }

    private static EvalContext Lambda(Environment env, ImmutableArray<Expression> expr)
    {
        var (pparameters, pbody) = expr switch
        {
            [List {Expressions: var parameters}, .. var body] => (parameters, body),
            [var parameter, .. var body] => ([parameter], body),
            _ => throw SyntaxError("'lambda'", expr)
        };

        return new EvalContext(env, new Special(Closure));

        EvalContext Closure(Environment callerEnv, ImmutableArray<Expression> args)
        {
            env = env.SetItems(callerEnv);
            
            var bindings = Zip(pparameters, args);
            return Eval(Fold(env, Bind, bindings), WrapBegin(pbody));

            Environment Bind(Environment penv, (Expression, Expression, bool) parg) =>
                parg switch
                {
                    (Symbol {Value: var p}, List a, true) => 
                        a.Expressions.Select(x => Eval(callerEnv, x).Expression)
                            .AndThen(es => ExtendEnvironment([(p, ListExpr([..es]))], penv)),
                    (Symbol {Value: var p}, var a, _) => Eval(callerEnv, a)
                        .AndThen(e => ExtendEnvironment([(p, e.Expression)], penv)),
                    _ => throw SyntaxError("'lambda' parameter.", args)
                };
        }
    }

    private static Expression WrapBegin(ImmutableArray<Expression> exprs) =>
        exprs is [var e]
            ? e
            : ListExpr( exprs.Insert(0, new Symbol("begin")));

    private static EvalContext QuasiQuote(Environment env, ImmutableArray<Expression> exprs)
    {
        Expression MapUnquote(ImmutableArray<Expression> acc, ImmutableArray<Expression> es) => es switch
        {
            [var h, .. var t] => MapUnquote(acc.AddRange(Unquote(h)), t),
            [] => ListExpr(acc)
        };

        ImmutableArray<Expression> Unquote(Expression expr) => expr switch
        {
            List {Expressions: [Symbol {Value: "unquote-splicing"}, var e]} => Eval(env, e).AndThen(
                 t => t.Expression switch
                 {
                     List { Expressions: var es } => es,
                     var e => [e]
                 }),
            List {Expressions: [Symbol {Value: "unquote"}, var e]} => Eval(env, e).AndThen(
                t => (ImmutableArray<Expression>) [t.Expression]),
            List {Expressions: [Symbol {Value: "unquote"}, .. _]} m => throw SyntaxError(
                "unquote (too many args)", [m]),
            List {Expressions: var lst} => [MapUnquote([], lst)],
            _ => [expr]
        };

        if (exprs is [var e])
        {
            return new EvalContext(env, Unquote(e).AndThen(r => r is [var expression] ? expression : ListExpr(r)) );
        }

        throw SyntaxError("'quasiquote'", exprs);
    }
    
    private static EvalContext Quote(Environment env, ImmutableArray<Expression> exprs)
    {
        if (exprs is [var e])
        {
            return new EvalContext(env, e);
        }

        throw SyntaxError("'quote'", exprs);
    }

    private static EvalContext Begin(Environment env, ImmutableArray<Expression> exprs) =>
        Fold(new EvalContext(env, DummyExpr), (ctx, e) => Eval(ctx.Environment, e), exprs);

    private static EvalContext Define(Environment env, ImmutableArray<Expression> exprs)
    {
        return exprs switch
        {
            [Symbol {Value: var sym}, var e] => InternalDefine(sym, e),
            [List { Expressions: [Symbol { Value: var sym }, .. var ps]}, .. var body] => 
                InternalDefine(sym, ListExpr([new Symbol("lambda"), ListExpr(ps), WrapBegin(body)])),
            [] => throw SyntaxError("'Define'", exprs)
        };

        EvalContext InternalDefine(string sym, Expression e)
        {
            var (penv, pe) = Eval(env, e);
            var ppenv = ExtendEnvironment([(sym, pe)], penv);
            return new EvalContext(ppenv, new DummyExpression($"Define {sym}"));
        }
    }

    private static EvalContext DefineMacro(Environment env, ImmutableArray<Expression> exprs)
    {
        if (exprs is not [List { Expressions: [Symbol { Value : var sym}, ..var parameters] }, var body])
        {
            throw SyntaxError("'define-macro'", exprs);
        }

        var penv = ExtendEnvironment([(sym, new Special(Closure))], env);
        return new EvalContext(penv, new DummyExpression($"Define Macro {sym}"));
        
        EvalContext Closure(Environment callerEnv, ImmutableArray<Expression> args)
        {
            var binding = Zip(parameters, args);
            return Eval(Fold(callerEnv, Bind, binding), body)
                .AndThen(ctx => Eval(ctx.Environment, ctx.Expression));

            Environment Bind(Environment penv, (Expression psym, Expression pexpr, bool _) map) =>
                map.psym is Symbol {Value: var s}
                    ? ExtendEnvironment([(s, map.pexpr)], penv)
                    : throw SyntaxError("'macro' parameter.", [map.pexpr]);
        }
    }

    private static EvalContext Set(Environment env, ImmutableArray<Expression> exprs) =>
        exprs is [Symbol {Value: var sym}, var e]
            ? Eval(env, e).AndThen(ctx =>
                // set! is dangerous because it alters the bindings table. Should I remove it???
                new EvalContext(env.SetItem(sym, ctx.Expression), new DummyExpression($"Set {sym}")))
            : throw SyntaxError("set!", exprs);

    private static EvalContext Eval(Environment env, ImmutableArray<Expression> exprs) =>
        exprs is [var args]
            ? Eval(env, args).AndThen(ctx => Eval(ctx.Environment, ctx.Expression))
            : throw SyntaxError("'eval'", exprs);

    private static Expression NullQm(ImmutableArray<Expression> es) =>
        es is [List {Expressions.Length: 0}] ? True : False;
    
    private static Expression Display(ImmutableArray<Expression> exprs)
    {
        if (exprs is [var e])
        {
            Console.Write(Print(e));
            return new DummyExpression("Dummy 'display'");
        }

        throw SyntaxError("'display'", exprs);
    }

    private static EvalContext Load(Environment env, ImmutableArray<Expression> exprs)
    {
        if (exprs is not [String {Value: var filename}])
        {
            throw SyntaxError("'load'", exprs);
        }

        var tokens = Tokenize(File.OpenText(filename).ReadToEnd());
        var parsingResults = Parse(tokens.ToArray());
        foreach (var result in parsingResults)
        {
            (env, _) = Eval(env, result);
        }

        return new EvalContext(env, new Symbol($"Loaded '{filename}'"));
    }

    public static EvalContext Load(Environment env, string filename) =>
        Load(env, [new String(filename)]);

    private static Boolean Is<T>(ImmutableArray<Expression> xs) => xs is [T] ? True : False;

    public static Environment DefineNativeFunction(string fname, Func<object> fn, Environment env) =>
        InternalDefineNativeFunction(fname, os => fn(), env);
    
    public static Environment DefineNativeFunction<T>(string fname, Func<T, object> fn, Environment env) =>
        InternalDefineNativeFunction(fname, os => fn((T)os[0]), env);
    
    public static Environment DefineNativeFunction<T0, T1>(string fname, Func<T0, T1, object> fn, Environment env) =>
        InternalDefineNativeFunction(fname, os => fn((T0)os[0], (T1)os[1]), env);
    
    private static Environment InternalDefineNativeFunction(string fname, Func<object[], object> fn, Environment env)
    {
        Expression MapNativeType(object obj) =>
            obj switch
            {
                int intValue => new Number(intValue),
                string stringValue => new String(stringValue),
                bool booleanValue => booleanValue ? True : False,
                IEnumerable<object> e => ListExpr(e.Select(MapNativeType).ToImmutableArray()),
                var o => new NativeObject(o)
            };
        
        object MapExpressionToNative(Expression e) =>
            e switch
            {
                Number { Value: var intValue } => intValue,
                String { Value: var stringValue } => stringValue,
                Boolean { Bool: var boolValue } => boolValue,
                NativeObject { Value: var objValue } => objValue,
                Symbol { Value: var symValue } => symValue,
                List { Expressions: var exprs } => exprs.Select(MapExpressionToNative),
            };
            
        Expression MapNativeTypes(ImmutableArray<Expression> acc, ImmutableArray<object> objs) =>
            objs switch
            {
                [var h, .. var t] => MapNativeType(h).AndThen(x => MapNativeTypes(acc.Add(x), t)),
                [] => ListExpr(acc)
            };

        ImmutableArray<object> MapExpressionsToNative(ImmutableArray<object> acc, ImmutableArray<Expression> exprs) =>
            exprs switch
            {
                [var h, .. var t] => MapExpressionToNative(h).AndThen(x => MapExpressionsToNative(acc.Add(x), t)),
                [] => acc
            };

        Expression WrapNativeFunction(ImmutableArray<Expression> exprs)
        {
            var parameters = MapExpressionsToNative([], exprs);
            var result = fn.Invoke(parameters.ToArray());
            var nativeResult = MapNativeTypes([], [result]);
            return nativeResult is List {Expressions: [var singleElement]}
                ? singleElement
                : nativeResult;
        }
        var fnative = new Function(WrapNativeFunction);
        
        return env.Add(fname, fnative);
    }
    
    public static readonly Environment Env = new Dictionary<string, Expression> {
        { "*", new Function(Multiply) },
        { "/", new Function(Divide) },
        { "%", new Function(Modulus) },
        { "+", new Function(Add) },
        { "-", new Function(Subtract) },
        { "=", new Function(NumericEquality) },
        { "eq?", new Function(IdentityEquality) },
        { "equal?", new Function(CompareStructuralEquality) },
        { ">", new Function(Greater) },
        { "<", new Function(Less) },
        { "null?", new Function(NullQm) },
        { "if", new Special(If) },
        { "let", new Special(Let) },
        { "letrec", new Special(LetRec) },
        { "let*", new Special(LetStar) },
        { "lambda", new Special(Lambda) },
        { "cons", new Function(Cons) },
        { "car", new Function(Car) },
        { "cdr", new Function(Cdr) },
        { "quote", new Special(Quote) },
        { "quasiquote", new Special(QuasiQuote) },
        { "unquote", new Function(e => DummyExpr)},
        { "eval", new Special(Eval) },
        { "define-macro", new Special(DefineMacro) },
        { "set!", new Special(Set) },
        { "begin", new Special(Begin) },
        { "define", new Special(Define) },
        { "load", new Special(Load) },
        { "display", new Function(Display) },
        { "number?", new Function(Is<Number>) },
        { "string?", new Function(Is<String>) },
        { "boolean?", new Function(Is<Boolean>) },
        { "symbol?", new Function(Is<Symbol>) },
        { "list?", new Function(Is<List>) },
        { "not", new Function(e => e is [Boolean { Bool: false }] ? True : False)}
    }.ToImmutableDictionary();

    private static IEnumerable<(Expression, Expression, bool)> Zip(ImmutableArray<Expression> ps,
        ImmutableArray<Expression> args)
    {
        return ZipDotted([], ps, args);

        ImmutableArray<(Expression, Expression, bool)> ZipDotted(ImmutableArray<(Expression, Expression, bool)> acc,
            ImmutableArray<Expression> pps, ImmutableArray<Expression> pas) =>
            (pps, pas) switch
            {
                ([Symbol("."), Symbol p, ..], var tas) => acc.Add((p, ListExpr(tas), true)),
                ([Symbol p, .. var tps], [var a, .. var tas]) => acc.Add((p, a, false)).AndThen(a => ZipDotted(a, tps, tas)),
                ([],[]) => acc,
                _ => throw SyntaxError($"{ps.Length} parameters were expected but {args.Length} were passed.", ps)
            };
    }

    private static SyntaxException SyntaxError(string msg, ImmutableArray<Expression> e) =>
        new($"[Syntax error] {msg} {Print(ListExpr(e))}");
    
    public class SyntaxException(string msg) : Exception(msg);

    private static TAccumulator Fold<TSource, TAccumulator>(TAccumulator acc,
        Func<TAccumulator, TSource, TAccumulator> fn, IEnumerable<TSource> p) =>
        p.Aggregate(acc, fn);
}

[DebuggerStepThrough]
public static class FunctionExtensions
{
    public static T AndThen<R,T>(this R me, Func<R, T> then) => then(me);
}
