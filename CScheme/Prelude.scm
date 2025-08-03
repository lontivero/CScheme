(define-macro (cond h . t)
               (if (null? h)
                  nil
                  (begin
                    ;(define h (car args))
                    ;(define t (cdr args))
                    (define test1 (if (equal? (car h) 'else) '#t (car h)))
                    (define expr1 (car (cdr h)))
                    `(if ,test1 ,expr1 (cond ,@t)))))

; (define-macro  (let* bindings . body)
;   (if (null? bindings) 
;     `((lambda () ,body))
;     `(let (,(car bindings))
;        (let* ,(cdr bindings) ,body))))

; logical 'and', 'or', 'not', 'xor'
(define (all . xs)(not (member? #f xs)))
(define (any . xs)(member? #t xs))

(define (find pred lst)
  (cond
    ((null? lst) #f)
    ((pred (car lst)) (car lst))
    (else (find pred (cdr lst)))))

(define (xor a b) (and (or a b) (not (and a b))))

(define nil '())

; map function (f) over list (xs)
(define (map f xs)
  (if (null? xs)
    nil
    (cons (f (car xs)) (map f (cdr xs)))))

(define (list . xs) xs)

(define (list? xs)
  (or (null? xs) (list? (cdr xs))))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

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
(define (even? x) (not (odd? x)))
(define (false? x) (not x))
(define (max x . rest)
  (if (null? rest)
    x
    (let ((m (apply max rest)))
      (if (> x m) x m))))

(define (min x . rest)
  (if (null? rest)
    x
    (let ((m (apply min rest)))
      (if (< x m) x m))))

(define (numerator x)
  (if (integer? x)
    x
    (let ((rat (exact x)))
      (if (integer? rat)
        rat
        (* (sign rat) (car (normalize rat)))))))

(define (denominator x)
  (if (integer? x)
    1
    (let ((rat (exact x)))
      (if (integer? rat)
        1
        (cadr (normalize rat))))))

(define (gcd . args)
  (cond ((null? args) 0)
    ((null? (cdr args)) (abs (car args)))
    (else (let ((a (car args))
                 (b (cadr args))
                 (rest (cddr args)))
            (apply gcd (cons (gcd-two-numbers a b) rest))))))

(define (gcd-two-numbers a b)
  (if (= b 0)
    (abs a)
    (gcd-two-numbers b (remainder a b))))

(define (lcm . args)
  (cond ((null? args) 1)
    ((null? (cdr args)) (abs (car args)))
    (else (let ((a (car args))
                 (b (cadr args))
                 (rest (cddr args)))
            (apply lcm (cons (lcm-two-numbers a b) rest))))))

(define (lcm-two-numbers a b)
  (if (or (= a 0) (= b 0))
    0
    (abs (/ (* a b) (gcd a b))))) 
  
(define (length xs) 
  (if (null? xs) 0 
      (+ 1 (length (cdr xs)))))

(define (range l r)
  (if (= l r) '() 
      (cons l (range (+ 1 l) r))))

(define (filter f xs) 
  (if (null? xs) '() 
      (if (f (car xs)) 
        (cons (car xs) (filter f (cdr xs))) 
        (filter f (cdr xs)))))

(define (member item lst)
  (if (null? lst) #f
      (if (equal? item (car lst)) lst
          (member item (cdr lst)))))

(define (memq item lst)
  (if (null? lst) #f
      (if (eq? item (car lst)) lst
          (memq item (cdr lst)))))

(define (list-tail lst k)
  (if (zero? k)
      lst
      (list-tail (cdr lst) (- k 1))))

(define (assq key alist)
  (cond ((null? alist) #f)
    ((eq? key (caar alist)) (car alist))
    (else (assq key (cdr alist)))))

(define (assoc key alist)
  (cond ((null? alist) #f)
    ((equal? key (caar alist)) (car alist))
    (else (assq key (cdr alist)))))

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

(define (caar xs) (car (car xs)))
(define (cddr xs) (cdr (cdr xs)))

(define first car)
(define second cadr)
(define third caddr)

(define-macro (while test body)
    `(letrec
       ((loop
          (lambda ()
            (if ,test
              (begin ,body (loop))
              nil))))
       (loop)))

(define newline (lambda () (display "\r\n")))
(define (write text) (display text))
(define (writeln text) (display text) (newline))
