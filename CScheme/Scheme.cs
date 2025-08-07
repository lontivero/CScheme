using System.Collections.Immutable;
using System.Diagnostics;
using System.Globalization;
using CScheme;
using static CScheme.Tokenizer;

using Environment = System.Collections.Immutable.ImmutableDictionary<string, CScheme.Expression>;
[assembly: DebuggerDisplay("{Scheme.Print(this),ng}", Target = typeof(Expression))]

namespace CScheme;

public abstract record Expression;

public record EvalContext(Environment Environment, Expression Expression);


public class Scheme
{
    private static Expression Lookup(string symbol, Environment env) =>
        env.TryGetValue(symbol, out var value)
            ? value
            : throw new InvalidOperationException($"No binding for '{symbol}'.");

    private static Environment ExtendEnvironment(IEnumerable<(string s, Expression e)> bindings, Environment env) =>
        env.SetItems(bindings.Select(x => KeyValuePair.Create(x.s, x.e)));

    private delegate Expression ExpressionsProcessor(Expression args);

    private delegate EvalContext SpecialExpressionsProcessor(Environment env, Expression args);

    private record Number(decimal Value) : Expression;

    private record String(string Value) : Expression;
    private record Character(string Value) : Expression;

    private record Boolean(bool Bool) : Expression;

    private record Symbol(string Value) : Expression;

    private abstract record List : Expression;

    private record Pair(Expression Car, Expression Cdr) : List;

    private record DottedList(Expression Car, Expression Cdr) : Pair(Car, Cdr);

    private record Nil : List;

    private record Function(ExpressionsProcessor Fn) : Expression;

    private record Procedure(SpecialExpressionsProcessor Fn) : Expression;

    private record NativeObject(object Value) : Expression;

    private record DummyExpression(string Val) : Expression;

    private static readonly Symbol QuoteExpr = new("quote");
    private static readonly Symbol QuasiQuoteExpr = new("quasiquote");
    private static readonly Symbol UnquoteExpr = new("unquote");
    private static readonly Symbol UnquoteSplicingExpr = new("unquote-splicing");
    private static readonly Boolean True = new(true);
    private static readonly Boolean False = new(false);
    private static readonly Nil NilExpr = new();
    private static Pair Cons(Expression Car, Expression Cdr) => new(Car, Cdr);

    private static Expression MapTokenToExpression(Token token) =>
        token switch
        {
            NumberToken n => new Number(decimal.Parse(n.number)),
            StringToken s => new String(s.str),
            CharacterToken c => new Character(c.c),
            BooleanToken s => s.b ? True : False,
            SymbolToken t => new Symbol(t.symbol),
            _ => throw new SyntaxException($"token '{token}' is not a mappeable to an expression.")
        };

    public static bool Trace = false;
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
                    [DotToken, .. var t] => ParseExpression(t)
                        .AndThen(r =>  r.UnparsedTokens is [CloseToken, .. var tokensToParse] 
                            ? (r.ParsedExpression, tokensToParse)
                            : throw SyntaxError("Incomplete form", NilExpr)),
                    
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
                Function f => ApplyArgs(ctx.Environment, f.Fn, t),
                Procedure f => f.Fn(ctx.Environment, t),
                _ => throw SyntaxError("The first element in a list must be a function", ctx.Expression)
            }),
            var e => new EvalContext(env, e)
        };
        if(Trace) Console.WriteLine($"{Print(expr)}    => {Print(r.Expression)}");
        return r;
    }

    private static EvalContext Evaluate(Environment env, Expression exprs) =>
        Eval(env, Car(exprs)).AndThen(ctx =>
            Eval(ctx.Environment, ctx.Expression));

    private static EvalContext ApplyArgs(Environment env, ExpressionsProcessor function, Expression args) =>
        Map(e => Eval(env, e).Expression, args)
            .AndThen(ars => new EvalContext(env, function(
                ars is Pair {Car: var car, Cdr: Nil} ? car : ars)));

    public static string Print(Expression expr) =>
        expr switch
        {
            Nil => "'()",
            List lst => $"({PrintList(lst)})",
            String str => str.Value,
            Symbol sym => sym.Value,
            Number num => num.Value.ToString(),
            Boolean b => b.Bool ? "#t" : "#f",
            Character c => $"#\\{c.Value}",
            Function or Procedure => "procedure",
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

    private static ExpressionsProcessor Math(decimal identity, decimal unary, Func<decimal, decimal, decimal> op) =>
        es => es switch
        {
            Nil => new Number(identity),
            Number n => new Number(unary * n.Value),
            Pair {Car: Number {Value: var n}, Cdr: var ns} => new Number(Fold(n,
                (acc, e) => op(acc, e is Number {Value: var nn} ? nn : throw SyntaxError("ss", e)), ns)),
            _ => throw SyntaxError("Math can only involve number", es)
        };

    private static ExpressionsProcessor Compare(Func<decimal, decimal, bool> op) =>
        es => es is Pair {Car: Number a, Cdr: Pair {Car: Number b}}
            ? op(a.Value, b.Value) ? True : False
            : throw SyntaxError("Binary comparison requires two expressions", es);

    private static ExpressionsProcessor NumericEquality =>
        es => es is Pair p 
            ? SameType<Number>(p, (a, b) => a.Value == b.Value) ? True : False
            : throw SyntaxError("Binary comparison requires two expressions", es);

    private static bool SameType<T>(Pair p, Func<T, T, bool> fn) =>
        p is {Car: T a, Cdr: Pair {Car: T b}} && fn(a, b);
    
    private static ExpressionsProcessor IdentityEquality =>
        es => es switch
        {
            Pair p when SameType<Number>(p, (a, b) => a.Value == b.Value) => True,
            Pair p when SameType<String>(p, (a, b) => a.Value == b.Value) => True,
            Pair p when SameType<Character>(p, (a, b) => a.Value == b.Value) => True,
            Pair p when SameType<Boolean>(p, (a, b) => a.Bool == b.Bool) => True,
            Pair p when SameType<Symbol>(p, (a, b) => a.Value == b.Value) => True,
            Pair p when SameType<Pair>(p, ReferenceEquals) => True,
            Pair p when SameType<Function>(p, (a, b) => ReferenceEquals(a.Fn, b.Fn)) => True,
            Pair p when SameType<Procedure>(p, (a, b) => ReferenceEquals(a.Fn, b.Fn)) => True,
            Pair p when SameType<Nil>(p, (_, _) => true) => True,
            Pair {Cdr: Pair} => False,
            _ => throw SyntaxError("Binary comparison requires two expressions", es)
        };

    private static ExpressionsProcessor CompareStructuralEquality =>
        es => es switch
        {
            Pair p when SameType<Number>(p, (a, b) => a.Value == b.Value) => True,
            Pair p when SameType<String>(p, (a, b) => a.Value == b.Value) => True,
            Pair p when SameType<Character>(p, (a, b) => a.Value == b.Value) => True,
            Pair p when SameType<Boolean>(p, (a, b) => a.Bool == b.Bool) => True,
            Pair p when SameType<Symbol>(p, (a, b) => a.Value == b.Value) => True,
            Pair p when SameType<Function>(p, (a, b) => a.Fn == b.Fn) => True,
            Pair p when SameType<Procedure>(p, (a, b) => a.Fn == b.Fn) => True,
            Pair p when SameType<Pair>(p, (a, b) => a == b) => True,
            Pair p when SameType<Nil>(p, (_, _) => true) => True,
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
        es is Pair {Car: var car} ? car : throw SyntaxError("'car'", es);

    private static Expression Cdr(Expression es) =>
        es is Pair {Cdr: var cdr} ? cdr : throw SyntaxError("'cdr'", es);

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

    private static EvalContext LetRec(Environment env, Expression exprs)
    {
        if (exprs is not Pair {Car: var bindings, Cdr: var body})
            throw SyntaxError("'let' must have bindings and a body expression.", exprs);

        return Eval(Fold(env, Bind, bindings), WrapBegin(body)) with {Environment = env};

        Environment Bind(Environment ppenv, Expression binding) =>
            binding is Pair {Car: Symbol {Value: var sym}, Cdr: var e}
                ? Eval(ppenv, Car(e)).AndThen(r => ExtendEnvironment([(sym, r.Expression)], r.Environment))
                : throw SyntaxError("'let' binding.", binding);
    }

    private static EvalContext Lambda(Environment env, Expression expr)
    {
        if (expr is not Pair {Car: var parameters, Cdr: var body})
            throw SyntaxError("'lambda'", expr);

        return new EvalContext(env, new Procedure(Closure));

        EvalContext Closure(Environment callerEnv, Expression args)
        {
            var penv = env.SetItems(callerEnv);
            var bindings = Zip(parameters, args);
            return Eval(Fold(penv, Bind, bindings), WrapBegin(body)) with{ Environment = callerEnv};

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

    private static Expression IsFalse(Expression expr) => expr is Boolean { Bool: false } ? True : False;
    
    private static EvalContext Test(Environment env, Expression exprs, Boolean expected) =>
        Find(e => Eval(env, e).AndThen(ctx => IsFalse(ctx.Expression) == expected), exprs)
            .AndThen(r => r is Nil ? expected : r)
            .AndThen(r => Eval(env, r));
    private static EvalContext And(Environment env, Expression exprs) =>
        Test(env, exprs, True);

    private static EvalContext Or(Environment env, Expression exprs) =>
        Test(env, exprs, False);
    
    private static Expression WrapBegin(Expression exprs) =>
        exprs switch
        {
            Pair {Car: var car, Cdr: Nil} => car,
            Pair {Car: Pair} p => Cons(new Symbol("begin"), p),
            var e => e
        };

    private static EvalContext QuasiQuote(Environment env, Expression exprs)
    {
        var u = Unquote(exprs);
        return new EvalContext(env, Unquote(exprs));

        Expression Unquote(Expression expr) =>
            expr switch
            {
                Pair { Car: Symbol("unquote"), Cdr: var e} => Eval(env, e).Expression,
                Pair { Car: Symbol("unquote-splicing"), Cdr: var e} => Eval(env, e).Expression,
                Pair { Car: var car, Cdr: var cdr } =>
                    car is Pair { Car: Symbol("unquote-splicing") }
                        ? Append(Unquote(car), Unquote(cdr))
                        : Cons(Unquote(car), Unquote(cdr)),
                var e => e
            };

        Expression Append(Expression p1, Expression p2) =>
            p1 switch
            {
                Nil => p2,
                Pair p => Cons(p.Car, Append(p.Cdr, p2))
            };
    }

    private static EvalContext Quote(Environment env, Expression exprs) =>
        new(env, exprs);

    private static EvalContext Begin(Environment env, Expression exprs) =>
        Fold(new EvalContext(env, NilExpr), (ctx, e) => Eval(ctx.Environment, e), exprs); // with{ Environment = env};

    private static EvalContext Define(Environment env, Expression exprs) =>
        exprs switch
        {
            Pair {Car: Symbol {Value: var sym}, Cdr: Pair {Car: var e}} =>
                Eval(env, e)
                    .AndThen(ctx => ExtendEnvironment([(sym, ctx.Expression)], ctx.Environment))
                    .AndThen(penv => new EvalContext(penv, new DummyExpression($"Define {sym}"))),
            Pair {Car: Pair {Car: Symbol {Value: var sym}, Cdr: var ps}, Cdr: var body} =>
                Lambda(env, Cons(ps, body))
                    .AndThen(ctx => ExtendEnvironment([(sym, ctx.Expression)], ctx.Environment))
                    .AndThen(penv => new EvalContext(penv, new DummyExpression($"Define {sym}"))),
            _ => throw SyntaxError("'Define'", exprs)
        };

    private static EvalContext Apply(Environment env, Expression exprs)
    {
        if (exprs is not Pair {Car: Symbol {Value: var procName}, Cdr: var args})
        {
            throw SyntaxError("'apply'", exprs);
        }

        var proc = Lookup(procName, env);
        var evaluatedArgs = Map(e => Eval(env, e).Expression, args)
            .AndThen(ars => ars is Pair {Car: var car, Cdr: Nil} ? car : ars);
        
        return proc switch
        {
            Procedure p => p.Fn(env, evaluatedArgs),
            Function f => f.Fn(evaluatedArgs).AndThen(r => new EvalContext(env, r)),
            _ => throw SyntaxError("", exprs)
        };
    }

    private static EvalContext DefineMacro(Environment env, Expression exprs)
    {
        if (exprs is not Pair { Car: Pair{ Car: Symbol {Value: var sym }, Cdr: var parameters}, Cdr: Pair { Car: var body }})
        {
            throw SyntaxError("'define-macro'", exprs);
        }

        var penv = ExtendEnvironment([(sym, new Procedure(Closure))], env);
        return new EvalContext(penv, new DummyExpression($"Define Macro {sym}"));
        
        EvalContext Closure(Environment callerEnv, Expression args)
        {
            var binding = Zip(parameters, args);
            return Eval(Fold(callerEnv, Bind, binding), body)
                .AndThen(ctx => Eval(callerEnv, ctx.Expression) with{ Environment = callerEnv });

            Environment Bind(Environment penv, Expression pexpr) =>
                pexpr is Pair {Car: Symbol {Value: var s}, Cdr: var e}
                    ? ExtendEnvironment([(s, e)], penv)
                    : throw SyntaxError("'macro' parameter.", pexpr);
        }
    }

    private static int _genSymCounter;
    private static Expression GenSym(Expression exprs) =>
        exprs switch
        {
            Symbol {Value: var prefix} => new Symbol($"#:{prefix}{_genSymCounter++}"),
            String {Value: var prefix} => new Symbol($"#:{prefix}{_genSymCounter++}"),
            Nil => new Symbol($"#:g{_genSymCounter++}"),
            _ => throw SyntaxError("gensym", exprs)
        };
    
    private static ExpressionsProcessor Convert<TSource>(
        Func<TSource, Expression> transformer,
        string operationName) 
        where TSource : Expression =>
        expr =>
        expr is TSource source
            ? transformer(source)
            : throw SyntaxError(operationName, expr);
    
    private static ExpressionsProcessor StringToSymbol =
        Convert<String>(str => new Symbol(str.Value), "string->symbol"); 
    
    private static ExpressionsProcessor SymbolToString =
        Convert<Symbol>(sym => new String(sym.Value), "symbol->string"); 
    
    private static ExpressionsProcessor NumberToString =
        Convert<Number>(num => new String(num.Value.ToString(CultureInfo.InvariantCulture)), "number->string"); 
    
    private static Expression StringAppend(Expression exprs) =>
        exprs is Pair {Car: String {Value: var str1 }, Cdr: Pair { Car: String{ Value: var str2} }}
            ? new String($"{str1}{str2}")
            : throw SyntaxError("string-append", exprs);
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
                char characterValue => new Character(characterValue.ToString()),
                bool booleanValue => booleanValue ? True : False,
                IEnumerable<object> e => e.Select(ConvertNativeToScheme).Reverse().Aggregate((Expression)NilExpr, (acc, expr) => Cons(expr, acc)),
                var o => new NativeObject(o)
            };
        
        object ConvertSchemeToNative(Expression e) =>
            e switch
            {
                Number { Value: var intValue } => intValue,
                String { Value: var stringValue } => stringValue,
                Character { Value: var characterValue } => characterValue,
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
        var fnative = new Function(WrapNativeFunction);
        
        return env.Add(fname, fnative);
    }

    private static ImmutableArray<Expression> FlatPairChain(List p) =>
        Fold(ImmutableArray<Expression>.Empty, (acc, e) => acc.Add(e), p);
    
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
        { "if", new Procedure(If) },
        { "letrec", new Procedure(LetRec) },
        { "lambda", new Procedure(Lambda) },
        { "cons", new Function(Cons) },
        { "car", new Function(Car) },
        { "cdr", new Function(Cdr) },
        { "quote", new Procedure(Quote) },
        { "quasiquote", new Procedure(QuasiQuote) },
        { "eval", new Procedure(Evaluate) },
        { "define-macro", new Procedure(DefineMacro) },
        { "begin", new Procedure(Begin) },
        { "define", new Procedure(Define) },
        { "apply", new Procedure(Apply) },
        { "load", new Procedure(Load) },
        { "display", new Function(Display) },
        { "number?", new Function(Is<Number>) },
        { "string?", new Function(Is<String>) },
        { "symbol?", new Function(Is<Symbol>) },
        { "pair?", new Function(Is<Pair>) },
        { "procedure?", new Function( e => (Is<Function>(e).Bool || Is<Procedure>(e).Bool) ? True : False) },
        { "and", new Procedure( And)},
        { "or", new Procedure( Or)},
        { "gensym", new Function( GenSym)},
        { "string->symbol", new Function( StringToSymbol)},
        { "symbol->string", new Function( SymbolToString)},
        { "number->string", new Function( NumberToString)},
        { "string-append", new Function( StringAppend)}
        
    }.ToImmutableDictionary();

    private static List Zip(Expression ps, Expression args)
    {
        return ZipDotted(NilExpr, ps, args);

        List ZipDotted(List acc, Expression pps, Expression pas) =>
            (pps, pas) switch
            {
                (Pair { Car: Symbol p}, Pair { Car: var a}) => Cons(Cons(p, a), ZipDotted(acc, Cdr(pps), Cdr(pas))),
                (Symbol p, Pair { Cdr: not Nil } tas) => Cons(new DottedList(p, tas), acc),
                (Symbol p, var tas) => Cons(new DottedList(p, tas), acc),
                (Nil,Nil) => acc,
                _ => throw SyntaxError($"parameters were expected but were passed.", ps)
            };
    }

    private static Expression Map(ExpressionsProcessor fn, Expression s) =>
        s switch
        {
            Nil => NilExpr,
            Pair {Car: var h, Cdr: var t} => Cons(fn(h), Map(fn, t)),
            var o => fn(o)
        };
    
    private static Expression Find(Func<Expression, bool> predicate, Expression expr) =>
        expr switch
        {
            Nil => NilExpr,
            Pair { Car: var car, Cdr: Nil } => car,
            Pair { Car: var car, Cdr: var cdr } => predicate(car) ? car : Find(predicate, cdr),
            var e => predicate(e) ? e : NilExpr
        };

    private static T Fold<T>(T acc, Func<T, Expression, T> fn, Expression p) =>
        p switch
        {
            Nil => acc,
            Pair { Car: var car, Cdr: var cdr } => Fold(fn(acc, car), fn, cdr),
            var e => fn(acc, e)
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
