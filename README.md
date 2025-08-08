CScheme
=======

CScheme is an immutable Scheme dialect interpreter implemented in C#, intended to be used as a scripting engine 
.NET projects. Initially ported from the beautiful [FScheme project](https://github.com/AshleyF/FScheme)


It is a bytecode compiled language, and comes with a compiler and a bytecode interpreter. The language includes the typical Lisp-dialect features you'd expect, like proper closures, tail-call optimization, and macros. However, like Scheme, it prefers explicit boolean types, and a single namespace.

Design goals:

    Easy to embed and use in C# / .NET - no extra dependencies
    Safe - does not expose .NET libraries to user code unless desired
    Fast - or at least fast enough with the use of bytecode compilation :)
    AOT friendly - does not use Reflection.Emit so it can be used in pre-compiled environments (mobile, consoles)
    Extensible - supports macros and primitives, user primops and reflection coming soon

CSLisp is intended to be used as a library, embedded in another host program, and not a standalone executable. The compiler, bytecode interpreter, and runtime environment, are all easy to access and manipulate from host programs. Unit tests and REPL show how to interop with it.

Unlike most .NET Lisp implementations, CSLisp does not emit .NET bytecode, it loads text files only, and compiles to its own bytecode. This is intended for compatibility with ahead-of-time (AOT) compiled environments, such as mobile and console games, which do not allow for runtime .NET IL generation or use of Reflection.Emit.

Language implementation should be pretty readable and easy to extend. Compiler and bytecode design are heavily cribbed from influenced by Quinnec's "Lisp in Small Pieces" and Norvig's "Principles of Artificial Intelligence Programming" . Standing on the shoulders on giants. :)

This is very much a work in progress, so please pardon the dust, use at your own risk, and so on. :)
USAGE

```scheme
Welcome to CScheme
> (define create-new-user (lambda (name email groups) (list name email groups)))
> (define create-new-admin-user (lambda (name email) (list name email '("admin" "sudoer"))))
> (define get-user-name (lambda (p)(first p)))
> (define get-user-email (lambda (p)(second p)))
> (define get-user-groups (lambda (p)(third p)))
> (define is-user-member-of? (lambda (p g)(member? g (get-user-groups p))))
> (define is-user-admin? (lambda (p)(is-user-member-of? p "admin")))
> (define maintainer (create-new-admin-user "Lucas" "lucas@ontivero.org"))
> maintainer
(Lucas lucas@ontivero.org (admin sudoer))
> (get-user-email maintainer)
lucas@ontivero.org
> (is-user-admin? maintainer)
1
> (get-user-groups maintainer)
(admin sudoer)
> 
```
**Have fun!**
