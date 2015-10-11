#lang racket

(require "hw1-2.rkt")
(provide crazy2mul)

(define (val c)
  (cond ((equal? c 'z) 0)
        ((equal? c 'p) 1)
        ((equal? c 'n) -1)
        (else 0)))

(define (mull d l);digit * list -> list
  (define (mul c0 c1);digit * digit -> digit
    (cond ((equal? (* (val c0) (val c1) ) 0) 'z)
          ((equal? (* (val c0) (val c1) ) 1) 'p)
          ((equal? (* (val c0) (val c1) ) -1) 'n)))
  (let ([x d]) (map (lambda (y) (mul x y)) l)))

(define (crazy2mul c0 c1)
  (if (> (length c0) (length c1))
    (crazy2mul c1 c0);multiply long num shorter times for small optmz.
    (if (null? c0)
      'z
      (crazy2add (mull (car c0) c1) (cons 'z (crazy2mul (cdr c0) c1))))))
