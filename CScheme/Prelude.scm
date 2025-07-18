(define cond (macro (args)
               (if (null? args)
                  nil
                  (begin
                    (define h (car args))
                    (define t (cdr args))
                    (define test1 (if (equal? (car h) 'else) '#t (car h)))
                    (define expr1 (car (cdr h)))
                    '(if ,test1 ,expr1 (cond ,t))))))

; logical 'and', 'or', 'not', 'xor'
(define and (macro (a b) '(if ,a (if ,b #t #f) #f)))
(define or (macro (a b) '(if ,a #t (if ,b #t #f))))
(define not? (lambda (x) (if x #f #t)))
(define xor (lambda (a b) (and (or a b) (not? (and a b)))))

(define nil '())

; map function (f) over list (xs)
(define map (lambda (f xs)
  (if (null? xs)
      nill
      (cons (f (car xs)) (map f (cdr xs))))))

(define list (macro (xs) '(map eval (quote ,xs))))

; fold function (f) over list (xs) while accumulating (a)
(define fold (lambda (f a xs)
  (if (null? xs) a
      (fold f (f (car xs) a) (cdr xs)))
  ))

(define reverse (lambda (xs) (fold cons nil xs)))

(define newline (lambda () (display "\r\n")))

; simple continuation to top-level
(define escape nil)
; (call/cc (lambda (c) (set! escape c)))

; error mechanism - print message and break out to top-level
(define error (lambda (msg) (begin (display msg) (escape nil))))

(define >= (lambda (a b) (or (> a b) (= a b))))
(define <= (lambda (a b) (or (< a b) (= a b))))

(define zero? (lambda (x)(= 0 x)))
(define inc (lambda (x)(+ 1 x)))
(define dec (lambda (x)(- x 1)))
(define sum (lambda (xs) (fold + 0 xs)))
(define odd? (lambda (x) (% x 2)))
(define even? (lambda (x) (not? (odd? x))))

(define require (lambda (e) (if e e (amb))))

(define length (lambda (xs) (if (null? xs) 0 (+ 1 (length (cdr xs))))))
(define range (lambda (l r)(if (= l r) '() (cons l (range (+ 1 l) r)))))
(define filter (lambda (f xs) (if (null? xs) '() (if (f (car xs)) (cons (car xs) (filter (cdr xs) f)) (filter (cdr xs) f)))))

(define member? (lambda (item lst)
     (if (null? lst)
         #f
         (if (= item (car lst))
             #t
             (member? item (cdr lst)))
         )))

(define distinct? (lambda (lst)
    (if (null? lst)
         #t
         (if (member? (car lst) (cdr lst))
             #f
             (distinct? (cdr lst)))
         )))

(define exclude (lambda (items lst)
     (if (null? lst)
         ()
         (if (member? (car lst) items)
             (exclude items (cdr lst))
             (cons (car lst) (exclude items (cdr lst)))))))

(define cadr (lambda (xs) (car (cdr xs))))
(define caddr (lambda (xs) (cadr (cdr xs))))
(define cadddr (lambda (xs) (caddr (cdr xs))))
(define caddddr (lambda (xs) (cadddr (cdr xs))))
(define cadddddr (lambda (xs) (caddddr (cdr xs))))
(define caddddddr (lambda (xs) (cadddddr (cdr xs))))

(define first car)
(define second cadr)
(define third caddr)

(define while
  (macro (test body)
    '(letrec
       ((loop
          (lambda ()
            (if ,test
              (begin ,body (loop))
              nil))))
       (loop))))

