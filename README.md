CScheme
=======

Scheme interpreter in C# - Ported from the beautiful [FScheme project](https://github.com/AshleyF/FScheme)

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
