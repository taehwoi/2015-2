#lang racket
(define p0 '(p n z . n))

(define (val c)
  (cond ((equal? c 'z) 0)
        ((equal? c 'p) 1)
        ((equal? c 'n) -1)
        (else 0)))

(define (crazy2val p)
  (if (pair? p)
    (+ (* (crazy2val (cdr p)) 2) (val (car p)))
    (val p));base case
    )

(crazy2val p0)
