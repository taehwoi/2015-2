#lang racket

(require "crazy2.rkt")
(require "crazy2add.rkt")

(define (c2revsign c0)
  (let ([rev (lambda (c);a local util function that swaps single digit c2's sign
          (cond
            ((equal? c 'p) 'n)
            ((equal? c 'n) 'p)
            (else 'z)))])
    (if (not (pair? c0))
      (rev c0) 
      (cons (rev (car c0)) (c2revsign (cdr c0))))))

(define (ezc2mul c0 c1); mul(10 , 9) = 10 + mul (10 , 8)
  (if (equal? (crazy2val c1) 0)
    'z
    (if (> (crazy2val c1) 0)
      (crazy2add c0 (ezc2mul c0 (crazy2add c1 'n)))
      (ezc2mul (c2revsign c0) (c2revsign c1)))));ezc2mul(c2 c0) == (ezc2mul -c2 -c0)

(crazy2val (ezc2mul '(n n . n) 'n))
