using System.Collections.Immutable;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using static CScheme.Tokenizer;

using Environment = System.Collections.Immutable.ImmutableArray<System.Collections.Immutable.ImmutableDictionary<string, CScheme.Expression>>;

namespace CScheme;

public abstract record Expression;

public record EvalContext(Environment Environment, Expression Expression)
{
    public T AndThen<T>(Func<EvalContext, T> then) => then(this);
}

public class Interpreter
{
    private static Expression Lookup(string symbol, Environment env)
    {
        foreach (var frame in env)
        {
            if (frame.TryGetValue(symbol, out var value))
            {
                return value;
            }
        }

        throw new InvalidOperationException($"No binding for '{symbol}'.");
    }

    private static Environment ExtendEnvironment(IEnumerable<(string s, Expression e)> bindings, Environment env) =>
        env.Insert(0, bindings.ToImmutableDictionary(x => x.s, x => x.e));

    private delegate Expression ExpressionsProcessor(ImmutableArray<Expression> args);

    private delegate EvalContext SpecialExpressionsProcessor(Environment env, ImmutableArray<Expression> args);

    private record NumberExpression(long Number) : Expression;

    private record StringExpression(string String) : Expression;

    private record SymbolExpression(string Symbol) : Expression;

    private record ListExpression(ImmutableArray<Expression> Expressions) : Expression;

    private record FunctionExpression(ExpressionsProcessor Function) : Expression;

    private record SpecialExpression(SpecialExpressionsProcessor Function) : Expression;


    private record DummyExpression(string Val) : Expression;

    private static readonly SymbolExpression QuoteExpr = new("quote");
    private static readonly SymbolExpression UnquoteExpr = new("unquote");
    private static readonly DummyExpression DummyExpr = new("dummy for letrec");
    private static ListExpression ListExpr(params Expression[] exprs) => new([..exprs]);
    private static ListExpression ListExpr(ImmutableArray<Expression> exprs) => new(exprs);

    private static Expression MapTokenToExpression(Token token) =>
        token switch
        {
            NumberToken n => new NumberExpression(long.Parse(n.number)),
            StringToken s => new StringExpression(s.str),
            SymbolToken t => new SymbolExpression(t.symbol),
            _ => throw new SyntaxException($"token '{token}' is not a mappeable to an expression.")
        };

    private static (ImmutableArray<Expression>, Token[] rest) ParseList(
        Func<ImmutableArray<Expression>, ImmutableArray<Expression>> fn,
        Token[] rest, ImmutableArray<Expression> acc)
    {
        var (exprs, rest2) = Parse(ImmutableArray<Expression>.Empty, rest);
        return Parse(acc.Add(ListExpr(fn(exprs))), rest2);
    }

    public static (ImmutableArray<Expression>, Token[] rest) Parse(ImmutableArray<Expression> acc, Token[] tokens) =>
        tokens switch
        {
            [OpenToken, .. var t] => ParseList(e => e, t, acc),
            [CloseToken, .. var t] => (acc, t),
            [QuoteToken, OpenToken, .. var t] => ParseList(e => [QuoteExpr, ListExpr(e)], t, acc),
            [QuoteToken, var h, .. var t] => Parse(acc.Add(ListExpr(QuoteExpr, MapTokenToExpression(h))), t),
            [UnquoteToken, OpenToken, .. var t] => ParseList(e => [UnquoteExpr, ListExpr(e)], t, acc),
            [UnquoteToken, var h, .. var t] => Parse(acc.Add(ListExpr(UnquoteExpr, MapTokenToExpression(h))), t),
            [var h, .. var t] => Parse(acc.Add(MapTokenToExpression(h)), t),
            [] => (acc, []),
        };

    public static EvalContext Eval(Environment env, Expression expr) =>
        expr switch
        {
            NumberExpression num => new EvalContext(env, num),
            StringExpression str => new EvalContext(env, str),
            SymbolExpression sym => new EvalContext(env, Lookup(sym.Symbol, env)),
            ListExpression {Expressions: [var h, .. var t]} => Eval(env, h).AndThen(ctx => ctx.Expression switch
            {
                FunctionExpression f => Apply(ctx.Environment, f.Function, t),
                SpecialExpression f => f.Function(ctx.Environment, t),
            }),
            DummyExpression s => throw new InvalidOperationException($"Cannot evaluate dummy value '{s.Val}'"),
            var e => throw SyntaxError("Error", [e])
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
            ListExpression lst => $"({string.Join(" ", lst.Expressions.Select(Print))})",
            StringExpression str => str.String,
            SymbolExpression sym => sym.Symbol,
            NumberExpression num => num.Number.ToString(),
            FunctionExpression or SpecialExpression => "Function",
            DummyExpression => string.Empty,
            _ => throw new ArgumentOutOfRangeException(nameof(expr))
        };

    private static ExpressionsProcessor Math(long identity, long unary, Func<long, long, long> op) =>
        es => es switch
        {
            [] => new NumberExpression(identity),
            [NumberExpression n] => new NumberExpression(unary * n.Number),
            [NumberExpression n, .. var ns] => new NumberExpression(ns.Cast<NumberExpression>().Select(x => x.Number)
                .Aggregate(n.Number, op))
        };

    private static ExpressionsProcessor Compare(Func<long, long, bool> op) =>
        es => es switch
        {
            [NumberExpression a, NumberExpression b] => op(a.Number, b.Number)
                ? new NumberExpression(1)
                : new NumberExpression(0),
            _ => throw SyntaxError("Binary comparison requires two expressions", es)
        };

    private static ExpressionsProcessor CompareEquality =>
        es => es switch
        {
            [NumberExpression a, NumberExpression b] => a.Number == b.Number
                ? new NumberExpression(1)
                : new NumberExpression(0),
            [StringExpression a, StringExpression b] => a.String == b.String
                ? new NumberExpression(1)
                : new NumberExpression(0),
            _ => throw SyntaxError("Binary comparison requires two expressions", es)
        };
    
    private static readonly ExpressionsProcessor Add = Math(0L, 1L, (a, b) => a + b);
    private static readonly ExpressionsProcessor Subtract = Math(0L, -1L, (a, b) => a - b);
    private static readonly ExpressionsProcessor Multiply = Math(1L, 1L, (a, b) => a * b);
    private static readonly ExpressionsProcessor Divide = Math(1L, 1L, (a, b) => a / b);
    private static readonly ExpressionsProcessor Modulus = Math(1L, 1L, (a, b) => a % b);

    private static readonly ExpressionsProcessor Equal = CompareEquality;
    private static readonly ExpressionsProcessor Greater = Compare((a, b) => a > b);
    private static readonly ExpressionsProcessor Less = Compare((a, b) => a < b);

    private static Expression Car(ImmutableArray<Expression> es) =>
        es switch
        {
            [ListExpression {Expressions: [var e, .. _]}] => e,
            _ => throw SyntaxError("'car'", es)
        };

    private static Expression Cdr(ImmutableArray<Expression> es) => 
        es switch
        {
            [ListExpression {Expressions: [_, .. var t]}] => ListExpr(t),
            _ => throw SyntaxError("'cdr'", es)
        };

    private static Expression Cons(ImmutableArray<Expression> es) =>
        es switch
        {
            [{ } es1, ListExpression {Expressions: var es2}] => ListExpr(es2.Insert(0, es1)),
            _ => throw SyntaxError("'list'", es)
        };

    private static EvalContext If(Environment env, ImmutableArray<Expression> exprs) =>
        exprs switch
        {
            [var condition, var t, var f] => Eval(env, condition).AndThen(ctx => ctx.Expression switch
            {
                ListExpression {Expressions: []} or StringExpression {String: ""} => Eval(ctx.Environment,
                    f), // empty list or empty string is false
                NumberExpression {Number: 0} => Eval(ctx.Environment, f), // zero is false
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
                [ListExpression {Expressions: [SymbolExpression {Symbol: var s}, var e]}, .. var restBindings] =>
                    Eval(env, e).AndThen(ctx => MapBind(acc.Add((s, ctx.Expression)), restBindings, body)),
                [] => Eval(ExtendEnvironment(acc, env), body),
                _ => throw SyntaxError("'let' binding.", bindings)
            };

        return exprs switch
        {
            [ListExpression {Expressions: var bindings}, var body] => MapBind([], bindings, body),
            _ => throw SyntaxError("'let' must have bindings and a body expression.", exprs)
        };
    }

    private static EvalContext LetRec(Environment env, ImmutableArray<Expression> exprs)
    {
        if (exprs is not [ListExpression {Expressions: var bindings}, var body])
        {
            throw SyntaxError("'let' must have bindings and a body expression.", exprs);
        }

        var extendedEnv = ExtendEnvironment(bindings.Select(Bind), env);
        return MapUpdate(extendedEnv, bindings);

        static (string, Expression) Bind(Expression e) => e switch
        {
            ListExpression {Expressions: [SymbolExpression {Symbol: var s}, _]} => (s, DummyExpr),
            _ => throw SyntaxError("'letrec' binding", [e])
        };

        EvalContext MapUpdate(Environment penv, ImmutableArray<Expression> bindings)
        {
            EvalContext InternalMapUpdate(string s, Expression e, ImmutableArray<Expression> restBindings)
            {
                var (ppenv, expr) = Eval(penv, e);
                var pppenv = ExtendEnvironment([(s, expr)], ppenv);
                return MapUpdate(pppenv, restBindings);
            }

            return bindings switch
            {
                [ListExpression {Expressions: [SymbolExpression {Symbol: var s}, var e]}, .. var restBindings] =>
                    InternalMapUpdate(s, e, restBindings),
                [] => Eval(extendedEnv, body),
                _ => throw SyntaxError("'let' binding.", bindings)
            };
        }
    }

    private static EvalContext LetStar(Environment env, ImmutableArray<Expression> exprs)
    {
        EvalContext FoldBind(Environment penv, ImmutableArray<Expression> bindings, Expression body) =>
            bindings switch
            {
                [ListExpression {Expressions: [SymbolExpression {Symbol: var s}, var e]}, .. var restBindings] =>
                    Eval(penv, e).AndThen(ctx =>
                        FoldBind(ExtendEnvironment([(s, ctx.Expression)], ctx.Environment), restBindings, body)), // TODO: check what environment to use
                [] => Eval(penv, body),
                _ => throw SyntaxError("'let*' binding.", bindings)
            };

        return exprs switch
        {
            [ListExpression {Expressions: var bindings}, var body] => FoldBind(env, bindings, body),
            _ => throw SyntaxError("'let*' must have bindings and a body expression.", exprs)
        };
    }

    private static EvalContext Lambda(Environment env, ImmutableArray<Expression> expr)
    {
        if (expr is not [ListExpression {Expressions: var parameters}, var body])
        {
            throw SyntaxError("'lambda'", expr);
        }

        EvalContext Closure(Environment callerEnv, ImmutableArray<Expression> args)
        {
            return MapBind([], [..Zip(parameters, args)]);

            EvalContext MapBind(ImmutableArray<(string, Expression)> acc,
                ImmutableArray<(Expression, Expression)> pargs) =>
                pargs switch
                {
                    [(SymbolExpression {Symbol: var p}, var a), .. var t] => Eval(callerEnv, a)
                        .AndThen(ctx => MapBind(acc.Add((p, ctx.Expression)), t)),
                    [] => Eval(ExtendEnvironment(acc, callerEnv.AddRange(env)), body),
                    _ => throw SyntaxError("'lambda' parameter.", args)
                };
        }

        return new EvalContext(env, new SpecialExpression(Closure));
    }

    private static EvalContext Quote(Environment env, ImmutableArray<Expression> exprs)
    {
        Expression MapUnquote(ImmutableArray<Expression> acc, ImmutableArray<Expression> es) => es switch
        {
            [var h, .. var t] => MapUnquote(acc.Add(Unquote(h)), t),
            [] => ListExpr(acc)
        };

        Expression Unquote(Expression expr) => expr switch
        {
            ListExpression {Expressions: [SymbolExpression {Symbol: "unquote"}, var e]} => Eval(env, e).Expression,
            ListExpression {Expressions: [SymbolExpression {Symbol: "unquote"}, .. _]} m => throw SyntaxError(
                "unquote (too many args)", [m]),
            ListExpression {Expressions: var lst} => MapUnquote([], lst),
            _ => expr
        };

        if (exprs is [var e])
        {
            return new EvalContext(env, Unquote(e));
        }

        throw SyntaxError("'quote'", exprs);
    }

    private static EvalContext Begin(Environment env, ImmutableArray<Expression> exprs)
    {
        EvalContext FoldEval(Environment penv, ImmutableArray<Expression> es, Expression last) => es switch
        {
            [var h, .. var t] => Eval(penv, h).AndThen(ctx => FoldEval(ctx.Environment, t, ctx.Expression)),
            [] => new EvalContext(env, last)
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
            [SymbolExpression {Symbol: var sym}, var e] => InternalDefine(sym, e),
            [] => throw SyntaxError("'Define'", exprs)
        };
    }

    private static EvalContext Set(Environment env, ImmutableArray<Expression> exprs) =>
        exprs switch
        {
            [SymbolExpression {Symbol: var sym}, var e] => Eval(env, e).AndThen(ctx =>
                // set! is dangerous because it alters the bindings table. Should I remove it???
                new EvalContext(
                [
                    // Fixme: too ugly. Just replace the binding in the closer frame
                    env.SelectMany(x => x)
                        .ToLookup(x => x.Key, x => x.Value)
                        .ToImmutableDictionary(x => x.Key, x => x.First())
                        .SetItem(sym, ctx.Expression)
                ], new DummyExpression($"Set {sym}"))),
            _ => throw SyntaxError("set!", exprs)
        };

    private static EvalContext Eval(Environment env, ImmutableArray<Expression> exprs) =>
        exprs switch
        {
            [var args] => Eval(env, args).AndThen(ctx => Eval(ctx.Environment, ctx.Expression)),
            [] => throw SyntaxError("'eval'", exprs)
        };

    private static EvalContext Macro(Environment env, ImmutableArray<Expression> exprs)
    {
        if (exprs is not [ListExpression {Expressions: var parameters}, var body])
        {
            throw SyntaxError("'macro'", exprs);
        }

        EvalContext Closure(Environment callerEnv, ImmutableArray<Expression> args)
        {
            (string, Expression) Bind((Expression, Expression Arg) t)
            {
                if (t is (SymbolExpression {Symbol: var psym}, _))
                {
                    return (psym, t.Arg);
                }

                throw SyntaxError("macro parameters", args);
            }
            var penv = ExtendEnvironment(Zip(parameters, args).Select(Bind), callerEnv); 
            return Eval(penv, body).AndThen(ctx => Eval(ctx.Environment, ctx.Expression));
        }
        return new EvalContext(env, new SpecialExpression(Closure));
    }
    
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
        if (exprs is not [StringExpression {String: var filename}])
        {
            throw SyntaxError("'load'", exprs);
        }

        var tokens = Tokenize(File.OpenText(filename).ReadToEnd());
        var (parsingResults, _) = Parse([], tokens.ToArray());
        foreach (var result in parsingResults)
        {
            (env, _) = Eval(env, result);
        }

        return new EvalContext(env, new SymbolExpression($"Loaded '{filename}'"));
    }

    public static EvalContext Load(Environment env, string filename) =>
        Load(env, [new StringExpression(filename)]);

    private static (Environment Env, string Result) Rep(Environment env, string prg)
    {
        var (parsingResult, _) = Parse([], Tokenize(prg).ToArray());
        var (penv, expressionResult) = Eval(env, parsingResult[0]);
        return (penv, Print(expressionResult));
    }

    public static void Repl(Environment env)
    {
        void InternalRepl(Environment env)
        {
            while (true)
            {
                Console.Write("> ");
                var line = Console.ReadLine();
                if (string.IsNullOrWhiteSpace(line)) continue;
                try
                {
                    var (penv, result) = Rep(env, line);
                    if (!string.IsNullOrWhiteSpace(result))
                    {
                        Console.WriteLine(result);
                    }
                    env = penv;
                }
                catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                }
            }
        }

        InternalRepl(env);
    }
    
    public static readonly Environment Env = [new Dictionary<string, Expression> {
        { "*", new FunctionExpression(Multiply) },
        { "/", new FunctionExpression(Divide) },
        { "%", new FunctionExpression(Modulus) },
        { "+", new FunctionExpression(Add) },
        { "-", new FunctionExpression(Subtract) },
        { "=", new FunctionExpression(Equal) },
        { ">", new FunctionExpression(Greater) },
        { "<", new FunctionExpression(Less) },
        { "if", new SpecialExpression(If) },
        { "let", new SpecialExpression(Let) },
        { "letrec", new SpecialExpression(LetRec) },
        { "let*", new SpecialExpression(LetStar) },
        { "lambda", new SpecialExpression(Lambda) },
        { "cons", new FunctionExpression(Cons) },
        { "car", new FunctionExpression(Car) },
        { "cdr", new FunctionExpression(Cdr) },
        { "quote", new SpecialExpression(Quote) },
        { "eval", new SpecialExpression(Eval) },
        { "macro", new SpecialExpression(Macro) },
        { "set!", new SpecialExpression(Set) },
        { "begin", new SpecialExpression(Begin) },
        { "define", new SpecialExpression(Define) },
        { "load", new SpecialExpression(Load) },
        { "display", new FunctionExpression(Display) },
   //     { "call/cc", new SpecialExpression(CallCC) },
   //     { "amb", new SpecialExpression(Ambivalent) },
    }.ToImmutableDictionary()];

    private static IEnumerable<(Expression, Expression)> Zip(ImmutableArray<Expression> ps, ImmutableArray<Expression> args) =>
        ps.Zip(ps.Length == args.Length
            ? args
            : args[..(ps.Length - 1)].Add(ListExpr(args[(ps.Length - 1)..])));

    private static SyntaxException SyntaxError(string msg, ImmutableArray<Expression> e) =>
        new($"[Syntax error] {msg} {Print(ListExpr(e))}");
    
    public class SyntaxException(string msg) : Exception(msg);
}

