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
    private static Expression Lookup(string symbol, Environment env)
    {
        if (env.TryGetValue(symbol, out var value))
        {
            return value;
        }

        throw new InvalidOperationException($"No binding for '{symbol}'.");
    }

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

    public record NativeObject(object Value) : Expression;

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
                // [] => ????
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

    private static EvalContext Apply(Environment env, ExpressionsProcessor function, ImmutableArray<Expression> args)
    {
        Expression MapEval(ImmutableArray<Expression> acc, ImmutableArray<Expression> args) =>
            args switch
            {
                [var h, .. var t] => Eval(env, h).AndThen(ctx => MapEval(acc.Add(ctx.Expression), t)),
                [] => function(acc)
            };

        return new EvalContext(env, MapEval([], args));
    }

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
        es => es switch
        {
            [Number a, Number b] => op(a.Value, b.Value) ? True : False,
            _ => throw SyntaxError("Binary comparison requires two expressions", es)
        };

    private static ExpressionsProcessor NumericEquality =>
        es => es switch
        {
            [Number a, Number b] => a.Value == b.Value ? True : False,
            _ => throw SyntaxError("Binary comparison requires two expressions", es)
        };

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
        es switch
        {
            [List {Expressions: [var e, .. _]}] => e,
            _ => throw SyntaxError("'car'", es)
        };

    private static Expression Cdr(ImmutableArray<Expression> es) => 
        es switch
        {
            [List {Expressions: [_, .. var t]}] => ListExpr(t),
            _ => throw SyntaxError("'cdr'", es)
        };

    private static Expression Cat(ImmutableArray<Expression> es)
    {
        if (es.All(e => e is List))
        {
            return ListExpr(es.Cast<List>().SelectMany(x => x.Expressions).ToImmutableArray());
        }

        throw SyntaxError("'cdr'", es);
    }

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
        EvalContext MapBind(ImmutableArray<(string, Expression)> acc, ImmutableArray<Expression> bindings,
            Expression body) =>
            bindings switch
            {
                [List {Expressions: [Symbol {Value: var s}, var e]}, .. var restBindings] =>
                    Eval(env, e).AndThen(ctx => MapBind(acc.Add((s, ctx.Expression)), restBindings, body)),
                [] => Eval(ExtendEnvironment(acc, env), body),
                _ => throw SyntaxError("'let' binding.", bindings)
            };

        return exprs switch
        {
            [List {Expressions: var bindings}, .. var body] => MapBind([], bindings, WrapBegin(body))
                .AndThen(ctx => ctx with {Environment = env}),
            _ => throw SyntaxError("'let' must have bindings and a body expression.", exprs)
        };
    }

    private static EvalContext LetRec(Environment env, ImmutableArray<Expression> exprs)
    {
        if (exprs is not [List {Expressions: var bindings}, .. var body])
        {
            throw SyntaxError("'let' must have bindings and a body expression.", exprs);
        }

        var extendedEnv = ExtendEnvironment(bindings.Select(Bind), env);
        return MapUpdate(extendedEnv, bindings);

        static (string, Expression) Bind(Expression e) => e switch
        {
            List {Expressions: [Symbol {Value: var s}, _]} => (s, DummyExpr),
            _ => throw SyntaxError("'letrec' binding", [e])
        };

        EvalContext MapUpdate(Environment penv, ImmutableArray<Expression> bindings)
        {
            EvalContext InternalMapUpdate(Environment penv, string s, Expression e, ImmutableArray<Expression> restBindings)
            {
                var (ppenv, expr) = Eval(penv, e);
                var pppenv = ExtendEnvironment([(s, expr)], ppenv);
                return MapUpdate(pppenv, restBindings);
            }

            return bindings switch
            {
                [List {Expressions: [Symbol {Value: var s}, var e]}, .. var restBindings] =>
                    InternalMapUpdate(penv, s, e, restBindings),
                [] => Eval(penv, WrapBegin(body)).AndThen(ctx => ctx with {Environment = env}),
                _ => throw SyntaxError("'let' binding.", bindings)
            };
        }
    }

    private static EvalContext LetStar(Environment env, ImmutableArray<Expression> exprs)
    {
        EvalContext FoldBind(Environment penv, ImmutableArray<Expression> bindings, ImmutableArray<Expression> body) =>
            bindings switch
            {
                [List {Expressions: [Symbol {Value: var s}, var e]}, .. var restBindings] =>
                    Eval(penv, e).AndThen(ctx =>
                        FoldBind(ExtendEnvironment([(s, ctx.Expression)], ctx.Environment), restBindings, body)), // TODO: check what environment to use
                [] => Eval(penv, WrapBegin(body)),
                _ => throw SyntaxError("'let*' binding.", bindings)
            };

        return exprs switch
        {
            [List {Expressions: var bindings}, .. var body] =>  FoldBind(env, bindings, body)
                .AndThen(ctx => ctx with {Environment = env}),
            _ => throw SyntaxError("'let*' must have bindings and a body expression.", exprs)
        };
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
            return MapBind([], [..Zip(pparameters, args)]);

            EvalContext MapBind(ImmutableArray<(string, Expression)> acc,
                ImmutableArray<(Expression, Expression)> pargs) =>
                pargs switch
                {
                    [(Symbol {Value: var p}, List a)] when pparameters is [.. _, Symbol("."), _] => 
                        ListExpr(a.Expressions.Select(x => Eval(callerEnv, x).Expression).ToImmutableArray())
                            .AndThen(ctx => MapBind(acc.Add((p, ctx)), [])),
                    [(Symbol {Value: var p}, var a), .. var t] => Eval(callerEnv, a)
                        .AndThen(ctx => MapBind(acc.Add((p, ctx.Expression)), t)),
                    [] => Eval(ExtendEnvironment(acc, env.SetItems(callerEnv)), WrapBegin(pbody)),
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

    private static EvalContext Begin(Environment env, ImmutableArray<Expression> exprs)
    {
        EvalContext FoldEval(Environment penv, ImmutableArray<Expression> es, Expression last) => es switch
        {
            [var h, .. var t] => Eval(penv, h).AndThen(ctx => FoldEval(ctx.Environment, t, ctx.Expression)),
            [] => new EvalContext(penv, last)
        };

        return FoldEval(env, exprs, new DummyExpression("Empty 'begin'"));
    }

    private static EvalContext Define(Environment env, ImmutableArray<Expression> exprs)
    {
        EvalContext InternalDefine(string sym, Expression e)
        {
            var (penv, pe) = Eval(env, e);
            var ppenv = ExtendEnvironment([(sym, pe)], penv);
            return new EvalContext(ppenv, new DummyExpression($"Define {sym}"));
        }

        return exprs switch
        {
            [Symbol {Value: var sym}, var e] => InternalDefine(sym, e),
            [List { Expressions: [Symbol { Value: var sym }, .. var ps]}, .. var body] => 
                InternalDefine(sym, ListExpr([new Symbol("lambda"), ListExpr(ps), WrapBegin(body)])),
            [] => throw SyntaxError("'Define'", exprs)
        };
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
            (string, Expression) Bind((Expression, Expression Arg) t)
            {
                if (t is (Symbol {Value: var psym}, _))
                {
                    return (psym, t.Arg);
                }

                throw SyntaxError("macro parameters", args);
            }
            var penv = ExtendEnvironment(Zip(parameters, args).Select(Bind), callerEnv); 
            return Eval(penv, body).AndThen(ctx => Eval(ctx.Environment, ctx.Expression));
        }
    }

    private static EvalContext Set(Environment env, ImmutableArray<Expression> exprs) =>
        exprs switch
        {
            [Symbol {Value: var sym}, var e] => Eval(env, e).AndThen(ctx =>
                // set! is dangerous because it alters the bindings table. Should I remove it???
                new EvalContext(env.SetItem(sym, ctx.Expression), new DummyExpression($"Set {sym}"))),
            _ => throw SyntaxError("set!", exprs)
        };

    private static EvalContext Eval(Environment env, ImmutableArray<Expression> exprs) =>
        exprs switch
        {
            [var args] => Eval(env, args).AndThen(ctx => Eval(ctx.Environment, ctx.Expression)),
            [] => throw SyntaxError("'eval'", exprs)
        };

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
        { "cat", new Function(Cat) },
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

    private static IEnumerable<(Expression, Expression)> Zip(ImmutableArray<Expression> ps,
        ImmutableArray<Expression> args)
    {
        return ZipDotted([], ps, args);

        ImmutableArray<(Expression, Expression)> ZipDotted(ImmutableArray<(Expression, Expression)> acc,
            ImmutableArray<Expression> pps, ImmutableArray<Expression> pas) =>
            (pps, pas) switch
            {
                ([Symbol("."), Symbol p, ..], var tas) => acc.Add((p, ListExpr(tas))),
                ([Symbol p, .. var tps], [var a, .. var tas]) => acc.Add((p, a)).AndThen(a => ZipDotted(a, tps, tas)),
                ([],[]) => acc,
                _ => throw SyntaxError($"{ps.Length} parameters were expected but {args.Length} were passed.", ps)
            };
    }

    private static SyntaxException SyntaxError(string msg, ImmutableArray<Expression> e) =>
        new($"[Syntax error] {msg} {Print(ListExpr(e))}");
    
    public class SyntaxException(string msg) : Exception(msg);
}

public static class FunctionExtensions
{
    [DebuggerStepThrough]
    public static T AndThen<R,T>(this R me, Func<R, T> then) => then(me);
}
