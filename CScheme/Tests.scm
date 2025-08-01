(load "Prelude.scm")

(define-macro (test expected expr)
  `(begin
    (newline)
    (display (if (equal? ,expected ,expr) "Passed " "!Failed \t"))
    (display (quote ,expr))
    nil))

(test 8 ((lambda (x) (+ x x)) 4))

; (test '(3 4 5 6) ((lambda x x) 3 4 5 6))

(test 'yes (if (> 3 2) 'yes 'no))

(test 'no (if (> 2 3) 'yes 'no))

(test 1 (if (> 3 2) (- 3 2) (+ 3 2)))

(test 'greater (cond ((> 3 2) 'greater) ((< 3 2) 'less)))

(test 'equal (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)))

(test #t (and (= 2 2) (> 2 1)))

(test #f (and (= 2 2) (< 2 1)))

; (test '(f g) (and 1 2 'c '(f g)))

(test #t (and))

(test #t (or (= 2 2) (> 2 1)))

(test #t (or (= 2 2) (< 2 1)))

; (test '(b c) (or (memq 'b '(a b c)) (/ 3 0)))

(test 6 (let ((x 2) (y 3)) (* x y)))

(test 35 (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))))

(test 70 (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))))

(test -2 (let ()
            (define x 2)
            (define f (lambda () (- x)))
            (f)))

(define let*-def 1)
(let* () (define let*-def 2) #f)
(test 1 let*-def)
; 
; (test '#(0 1 2 3 4)
;   (do ((vec (make-vector 5))
;         (i 0 (+ i 1)))
;     ((= i 5) vec)
;     (vector-set! vec i i)))

; (test 25
;   (let ((x '(1 3 5 7 9)))
;     (do ((x x (cdr x))
;           (sum 0 (+ sum (car x))))
;       ((null? x)
;         sum))))
; 
; (test '((6 1 3) (-5 -2))
;   (let loop ((numbers '(3 -2 1 6 -5)) (nonneg '()) (neg '()))
;     (cond
;       ((null? numbers)
;         (list nonneg neg))
;       ((>= (car numbers) 0)
;         (loop (cdr numbers) (cons (car numbers) nonneg) neg))
;       ((< (car numbers) 0)
;         (loop (cdr numbers) nonneg (cons (car numbers) neg))))))

(test '(list 3 4) `(list ,(+ 1 2) 4))
 
(test '(list a 'a) (let ((name 'a)) `(list ,name ',name)))

(test #t (eq? 'a 'a))

(test #f (eq? (list 'a) (list 'a)))

(test #t (eq? '() '()))

(test #t (eq? car car))

(test #t (let ((x '(a))) (eq? x x)))

(test #t (let ((p (lambda (x) x))) (eq? p p)))

(test #t (equal? 'a 'a))

(test #t (equal? '(a) '(a)))

(test #t (equal? '(a (b) c) '(a (b) c)))

(test #t (equal? "abc" "abc"))

(test #f (equal? "abc" "abcd"))

(test #f (equal? "a" "b"))

(test #t (equal? 2 2))

(test 7 (+ 3 4))

(test 3 (+ 3))

(test 0 (+))

(test 4 (* 4))

(test 1 (*))

(test -1 (- 3 4))

(test -6 (- 3 4 5))

(test -3 (- 3))

; (test -1.0 (- 3.0 4))

(test 7 (abs -7))

(test 1 (modulo 13 4))

(test 1 (remainder 13 4))

(test 3 (modulo -13 4))

(test -1 (remainder -13 4))

(test -3 (modulo 13 -4))

(test 1 (remainder 13 -4))

(test -1 (modulo -13 -4))

(test -1 (remainder -13 -4))

; (test 4 (gcd 32 -36))

(test #f (not 3))

(test #f (not (list 3)))

(test #f (not '()))

(test #f (not (list)))

(test #f (not '()))

(test #f (boolean? 0))

(test #f (boolean? '()))

(test '(a) (cons 'a '()))

(test '((a) b c d) (cons '(a) '(b c d)))

(test '("a" b c) (cons "a" '(b c)))

; (test '(a . 3) (cons 'a 3))

; (test '((a b) . c) (cons '(a b) 'c))

(test 'a (car '(a b c)))

(test '(a) (car '((a) b c d)))

(test 1 (car '(1 . 2)))

(test '(b c d) (cdr '((a) b c d)))

; (test 2 (cdr '(1 . 2)))

(test #t (list? '(a b c)))

(test #t (list? '()))

; (test #f (list? '(a . b)))


; (test #f
;   (let ((x (list 'a)))
;     (set-cdr! x x)
;     (list? x)))

(test '(a 7 c) (list 'a (+ 3 4) 'c))

(test '() (list))

(test 3 (length '(a b c)))

(test 3 (length '(a (b) (c d e))))

(test 0 (length '()))

(test '(x y) (append '(x) '(y)))

(test '(a b c d) (append '(a) '(b c d)))

(test '(a (b) (c)) (append '(a (b)) '((c))))

;(test '(a b c . d) (append '(a b) '(c . d)))

(test 'a (append '() 'a))

(test '(c b a) (reverse '(a b c)))

(test '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

(test 'c (list-ref '(a b c d) 2))

(test '(a) (cons 'a '()))

(test '((a) b c d) (cons '(a) '(b c d)))

(test '("a" b c) (cons "a" '(b c)))

; (test '(a . 3) (cons 'a 3))

; (test '((a b) . c) (cons '(a b) 'c))

(test 'a (car '(a b c)))

(test '(a) (car '((a) b c d)))

(test 1 (car '(1 . 2)))

(test '(b c d) (cdr '((a) b c d)))

(test #t (symbol? 'foo))

(test #t (symbol? (car '(a b))))

(test #f (symbol? "bar"))

(test #t (symbol? 'nil))

(test #f (symbol? '()))

(test #t (string? "a"))

(test #f (string? 'a))

(letrec ((add (lambda (a b) (+ a b))))
  (write (add 3 4))
  (newline))

(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
          (odd? (lambda (n) (if (zero? n) #f (even? (- n 1))))))
  (write (even? 1000))
  (newline)
  (write (even? 1001))
  (newline)
  (write (odd? 1000))
  (newline)
  )
(newline)