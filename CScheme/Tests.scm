(load "Stdlib.scm")

(define-macro (test expected expr)
  `(begin
    (write (if (equal? ,expected ,expr) "Passed " "!Failed \t"))
    (writeln (quote ,expr))
    ))

(test 8 ((lambda (x) (+ x x)) 4))

(test '(3 4 5 6) ((lambda x x) 3 4 5 6))

(test 'yes (if (> 3 2) 'yes 'no))

(test 'no (if (> 2 3) 'yes 'no))

(test 1 (if (> 3 2) (- 3 2) (+ 3 2)))

(test 'greater (cond ((> 3 2) 'greater) ((< 3 2) 'less)))

(test 'equal (cond ((> 3 3) 'greater) ((< 3 3) 'less) (else 'equal)))

(test 'composite (case (* 2 3) ((2 3 5 7) 'prime) ((1 4 6 8 9) 'composite)))

(test 'consonant
  (case (car '(c d))
    ((a e i o u) 'vowel)
    ((w y) 'semivowel)
    (else 'consonant)))

(test #t (and (= 2 2) (> 2 1)))

(test #f (and (= 2 2) (< 2 1)))

(test '(f g) (and 1 2 'c '(f g)))

(test #t (and))

(test #t (or (= 2 2) (> 2 1)))

(test #t (or (= 2 2) (< 2 1)))

(test '(b c) (or (memq 'b '(a b c)) (/ 3 0)))

(test '(a b c) (memq 'a '(a b c)))

(test '(b c) (memq 'b '(a b c)))

(test #f (memq 'a '(b c d)))

(test #f (memq (list 'a) '(b (a) c)))

(test '((a) c) (member (list 'a) '(b (a) c)))

(test '(101 102) (memv 101 '(100 101 102)))

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
 (test '((6 1 3) (-5 -2))
   (let loop ((numbers '(3 -2 1 6 -5)) (nonneg '()) (neg '()))
     (cond
       ((null? numbers)
         (list nonneg neg))
       ((>= (car numbers) 0)
         (loop (cdr numbers) (cons (car numbers) nonneg) neg))
       ((< (car numbers) 0)
         (loop (cdr numbers) nonneg (cons (car numbers) neg))))))

(test '(list 3 4) `(list ,(+ 1 2) 4))
 
(test '(list a 'a) (let ((name 'a)) `(list ,name ',name)))

(test #t (eq? 'a 'a))

(test #f (eq? (list 'a) (list 'a)))

(test #t (eq? '() '()))

(test #t (eq? car car))

(test #t (eqv? 'a 'a))
(test #f (eqv? 'a 'b))
(test #t (eqv? 2 2))
(test #t (eqv? '() '()))
(test #t (eqv? '10000 '10000))
(test #f (eqv? (cons 1 2)(cons 1 2)))
(test #f (eqv? (lambda () 1) (lambda () 2)))
(test #f (eqv? #f 'nil))

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

(test -1.0 (- 3.0 4))

(test 7 (abs -7))

(test 1 (modulo 13 4))

(test 1 (remainder 13 4))

(test 3 (modulo -13 4))

(test -1 (remainder -13 4))

(test -3 (modulo 13 -4))

(test 1 (remainder 13 -4))

(test -1 (modulo -13 -4))

(test -1 (remainder -13 -4))

(test 0 (gcd))

(test 4 (gcd 32 -36))

(test 1 (lcm))

(test 288 (lcm 32 -36))

(test 2 (quotient 7 3))
(test -2 (quotient -7 3))
(test -2 (quotient 7 -3))
(test 2 (quotient -7 -3))

(test -5.0 (floor -4.3))
(test -4.0 (ceiling -4.3))
(test -4.0 (truncate -4.3))
(test -4.0 (round -4.3))
(test 3.0 (floor 3.5))
(test 4.0 (ceiling 3.5))
(test 3.0 (truncate 3.5))
(test 4.0 (round 3.5))
(test 7 (round 7))

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

(test '(a . 3) (cons 'a 3))

(test '((a b) . c) (cons '(a b) 'c))

(test 'a (car '(a b c)))

(test '(a) (car '((a) b c d)))

(test 1 (car '(1 . 2)))

(test '(b c d) (cdr '((a) b c d)))

(test 2 (cdr '(1 . 2)))

(test #t (list? '(a b c)))

(test #t (list? '()))

(define e '((a 1) (b 2) (c 3)))

(test '(a 1) (assq 'a e))

(test '(b 2) (assq 'b e))

(test #f (assq 'd e))

(test #f (assq (list 'a) '(((a)) ((b)) ((c)))))

(test '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))

(test '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

(test #f (list? '(a . b)))


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

(test '(a b c . d) (append '(a b) '(c . d)))

(test 'a (append '() 'a))

(test '(c b a) (reverse '(a b c)))

(test '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

(test 'c (list-ref '(a b c d) 2))
  
(test '(a) (cons 'a '()))

(test '((a) b c d) (cons '(a) '(b c d)))

(test '("a" b c) (cons "a" '(b c)))

(test '(a . 3) (cons 'a 3))

(test '((a b) . c) (cons '(a b) 'c))

(test 'a (car '(a b c)))

(test '(a) (car '((a) b c d)))

(test 1 (car '(1 . 2)))

(test '(b c d) (cdr '((a) b c d)))

(test #t (symbol? 'foo))

(test #t (symbol? (car '(a b))))

(test #f (symbol? "bar"))

(test #t (symbol? 'nil))

(test #f (symbol? '()))

(test #f (symbol? #f))

(test #t (string? "a"))

(test #f (string? 'a))

(test #t (procedure? car))

(test #f (procedure? 'car))

(test #t (procedure? (lambda (x) (* x x))))

(test #f (procedure? '(lambda (x) (* x x))))

(test 7 (apply + (list 3 4)))

(test '(b e h) (map cadr '((a b) (d e) (g h))))

(test '(1 4 27 256 3125) (map (lambda (n) (expt n n)) '(1 2 3 4 5)))

(test '(5 7 9) (map + '(1 2 3) '(4 5 6)))

(test 3 (force (delay (+ 1 2))))

(test '(3 3) (let ((p (delay (+ 1 2)))) (list (force p) (force p))))

(test 'ok (let ((else 1)) (cond (else 'ok) (#t 'bad))))

; (test '(,foo) (let ((unquote 1)) `(,foo)))

; (test '(,@foo) (let ((unquote-splicing 1)) `(,@foo)))

(test '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))

(test '(10 5 4 16 9 8) `(10 5 ,(expt 2 2) ,@(map (lambda (n) (expt n 2)) '(4 3)) 8))

; (test '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
;  `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))

; (test '(a `(b ,x ,'y d) e)
;  (let ((name1 'x)
;         (name2 'y))
;    `(a `(b ,,name1 ,',name2 d) e)))

; (test '(list 3 4)
;   (quasiquote (list (unquote (+ 1 2)) 4)))

(test #f (string=? (symbol->string 'a) (symbol->string 'A)))

(test #f 
  (or (string=? (symbol->string 'a) "A")
    (string=? (symbol->string 'A) "a")))

(test #t (number? 3))
(test #t (complex? 3))
(test #t (real? 3))
(test #t (rational? 3))
(test #t (integer? 3))
(test #t (exact? 3))
(test #f (inexact? 3))

(test 1 (expt 0 0))
(test 0 (expt 0 1))
(test 0 (expt 0 256))
(test 1 (expt -1 256))
(test -1 (expt -1 255))
(test 1 (expt -1 -256))
(test -1 (expt -1 -255))
(test 1 (expt 256 0))
(test 1 (expt -256 0))
(test 256 (expt 256 1))
(test -256 (expt -256 1))
(test 8 (expt 2 3))
(test -8 (expt -2 3))
(test 9 (expt 3 2))
(test 9 (expt -3 2))

(test #t (= 22 22 22))
(test #t (= 22 22))
(test #f (= 34 34 35))
(test #f (= 34 35))
(test #t (> 3 -6246))
(test #f (> 9 9 -2424))
(test #t (>= 3 -4 -6246))
(test #t (>= 9 9))
(test #f (>= 8 9))
(test #t (< -1 2 3 4 5 6 7 8))
(test #f (< -1 2 3 4 4 5 6 7))
(test #t (<= -1 2 3 4 5 6 7 8))
(test #t (<= -1 2 3 4 4 5 6 7))
(test #f (< 1 3 2))
(test #f (>= 1 3 2))

(test #t (zero? 0))
(test #f (zero? 1))
(test #f (zero? -1))
(test #f (zero? -100))
(test #t (positive? 4))
(test #f (positive? -4))
(test #f (positive? 0))
(test #f (negative? 4))
(test #t (negative? -4))
(test #f (negative? 0))
(test #t (odd? 3))
(test #f (odd? 2))
(test #f (odd? -4))
(test #t (odd? -1))
(test #f (even? 3))
(test #t (even? 2))
(test #t (even? -4))
(test #f (even? -1))

(test 38 (max 34 5 7 38 6))
(test -24 (min 3  5 5 330 4 -24))

(test 7 (+ 3 4))
(test '3 (+ 3))
(test 0 (+))
(test 4 (* 4))
(test 1 (*))
(test 1 (/ 1))
(test -1 (/ -1))
(test 2 (/ 6 3))
(test -3 (/ 6 -2))
(test -3 (/ -6 2))
(test 3 (/ -6 -2))
(test -1 (- 3 4))
(test -3 (- 3))
(test 7 (abs -7))
(test 7 (abs 7))
(test 0 (abs 0))

(test 3.9 (string->number "3.9"))
(test 4.0 (string->number "4.0"))
(test -3.25 (string->number "-3.25"))
(test .25 (string->number ".25"))
(test 4.5 (string->number "4.5"))
(test 3.5 (string->number "3.5"))
(test 0.0 (string->number "0.0"))
(test 0.8 (string->number "0.8"))
(test 1.0 (string->number "1.0"))

(test "0" (number->string 0))
(test "100" (number->string 100))

;(test "100" (number->string 256 16))
(test 100 (string->number "100"))
;(test 256 (string->number "100" 16))
(test #f (string->number ""))
(test #f (string->number "."))
(test #f (string->number "d"))
(test #f (string->number "D"))
(test #f (string->number "i"))
(test #f (string->number "I"))
(test #f (string->number "3i"))
(test #f (string->number "3I"))
(test #f (string->number "33i"))
(test #f (string->number "33I"))
(test #f (string->number "3.3i"))
(test #f (string->number "3.3I"))

(test "" (substring "ab" 0 0))
(test "" (substring "ab" 1 1))
(test "" (substring "ab" 2 2))
(test "a" (substring "ab" 0 1))
(test "b" (substring "ab" 1 2))
(test "ab" (substring "ab" 0 2))
(test "foobar" (string-append "foo" "bar"))
(test "foo" (string-append "foo"))
(test "foo" (string-append "foo" ""))
(test "foo" (string-append "" "foo"))

(test 0 (apply + '()))
(test 7 (apply + (list 3 4)))
; (test 7 (apply (lambda (a b) (+ a b)) (list 3 4)))
; (test 17 (apply + 10 (list 3 4)))
(test '() (apply list '()))

(writeln "letrec tests ---")
(letrec ((add (lambda (a b) (+ a b))))
  (test 7 (add 3 4)))

(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
          (odd? (lambda (n) (if (zero? n) #f (even? (- n 1))))))
  (test #t (even? 1000))
  (test #f (even? 1001))
  (test #f (odd? 1000))
  )

;(test '(1 2) (apply append '((1) (2))))

(writeln "Done!")