(define cond (macro (args)
               (if (null? args)
                  nil
                  (begin
                    (define h (car args))
                    (define t (cdr args))
                    (define test1 (if (equal? (car h) 'else) '#t (car h)))
                    (define expr1 (car (cdr h)))
                    `(if ,test1 ,expr1 (cond ,t))))))

; logical 'and', 'or', 'not', 'xor'
(define and (macro (a b) `(if ,a (if ,b #t #f) #f)))
(define or (macro (a b) `(if ,a #t (if ,b #t #f))))
(define (not? x) (if x #f #t))
(define (xor a b) (and (or a b) (not? (and a b))))

(define nil '())

; map function (f) over list (xs)
(define (map f xs)
  (if (null? xs)
    nil
    (cons (f (car xs)) (map f (cdr xs)))))

; (define list (macro (xs) '(map eval (quote ,xs))))
(define list (macro (xs) `(if (list? xs) (quote ,xs) (quote (,xs)))))

(define (append-two list1 list2)
  (cond
    ((null? list1) list2)
    (else
      (cons (car list1)
        (append-two (cdr list1) list2)))))

(define (append lists)
  (cond
    ((null? lists) '())
    ((null? (cdr lists)) (car lists))
    (else
      (let ((h (car lists))
            (t (append (cdr lists))))
        (append-two h t)))))

(define (list-ref lst n)
  (cond
    ((null? lst) #f)
    ((= n 0) (car lst))
    ((< n 0) #f)
    (else (list-ref (cdr lst) (- n 1)))))

(define (fold f a xs)
  (if (null? xs) a
      (fold f (f (car xs) a) (cdr xs))))

(define (reverse xs) (fold cons nil xs))

; simple continuation to top-level
(define escape nil)
; (call/cc (lambda (c) (set! escape c)))

; error mechanism - print message and break out to top-level
(define (error (msg)) (display msg) (escape nil))

(define (>= a b) (or (> a b) (= a b)))
(define (<= a b) (or (< a b) (= a b)))

(define (zero? x)(= 0 x))
(define (possitive? x)(>= x 0))
(define (negative? x)(< x 0))
(define (inc x)(+ 1 x))
(define (dec x)(- x 1))
(define (sum xs) (fold + 0 xs))
(define (abs x) (if (< x 0) (* -1 x) x))
(define (modulo x y) (% x y))
(define (remainder x y) (% x y))
(define (odd? x) (% x 2))
(define (even? x) (not? (odd? x)))

(define (length xs) 
  (if (null? xs) 0 
      (+ 1 (length (cdr xs)))))

(define (pair? xs) (> (length xs) 1))
                     
(define (range l r)
  (if (= l r) '() 
      (cons l (range (+ 1 l) r))))

(define (filter f xs) 
  (if (null? xs) '() 
      (if (f (car xs)) 
        (cons (car xs) (filter f (cdr xs))) 
        (filter f (cdr xs)))))

(define (member? item lst)
  (if (null? lst)
      #f
      (if (= item (car lst))
          #t
          (member? item (cdr lst)))))

(define (distinct? lst)
    (if (null? lst)
        #t
        (if (member? (car lst) (cdr lst))
            #f
            (distinct? (cdr lst)))))

(define (exclude items lst)
    (if (null? lst)
        nil
        (if (member? (car lst) items)
            (exclude items (cdr lst))
            (cons (car lst) (exclude items (cdr lst))))))

(define (cadr xs) (car (cdr xs)))
(define (caddr xs) (cadr (cdr xs)))
(define (cadddr xs) (caddr (cdr xs)))
(define (caddddr xs) (cadddr (cdr xs)))
(define (cadddddr xs) (caddddr (cdr xs)))
(define (caddddddr xs) (cadddddr (cdr xs)))

(define first car)
(define second cadr)
(define third caddr)

(define while
  (macro (test body)
    `(letrec
       ((loop
          (lambda ()
            (if ,test
              (begin ,body (loop))
              nil))))
       (loop))))

(define newline (lambda () (display "\r\n")))
(define (write text) (display text))
(define (writeln text) (display text) (newline))

