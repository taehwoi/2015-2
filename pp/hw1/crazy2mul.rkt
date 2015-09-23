#lang racket

(require "crazy2.rkt")
(require "crazy2add.rkt")

(define (c2revsign c0)
  ;a local util function that swaps single digit c2's sign
  (define (rev c)
          (cond
            ((equal? c 'p) 'n)
            ((equal? c 'n) 'p)
            (else 'z)))
    (if (not (pair? c0))
      (rev c0) 
      (cons (rev (car c0)) (c2revsign (cdr c0)))))

;depends on the size of input, while using digits depend on logN -> too slow
(define (ezc2mul c0 c1); mul(10 , 9) = 10 + mul (10 , 8)
  (if (< (crazy2val c0) (crazy2val c1))
    (ezc2mul c1 c0);add longer one shorter times
  (if (equal? (crazy2val c1) 0)
    'z
    (if (> (crazy2val c1) 0)
      (crazy2add c0 (ezc2mul c0 (crazy2add c1 'n)))
      (ezc2mul (c2revsign c0) (c2revsign c1))))));ezc2mul(c2 c0) == (ezc2mul -c2 -c0)

;tail recursive version
(define (tail_ezc2mul c0 c1 r)
  (if (equal? (crazy2val c1) 0)
    r
    (if (> (crazy2val c1) 0)
      (tail_ezc2mul c0 (crazy2add c1 'n) (crazy2add r c0))
      (tail_ezc2mul (c2revsign c0) (c2revsign c1) r))));ezc2mul(c2 c0) == (ezc2mul -c2 -c0)

(crazy2val (ezc2mul '(p p p p p p p p p . p) '(n n n n n n n n n n n n n p . p)))
;(crazy2val (ezc2mul 'p '(p p p p p p p p p p p p p p . p)))
