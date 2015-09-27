#lang racket

(require "crazy2.rkt")
(require "crazy2add.rkt")

(define (car! c)
  (if (pair? c)
    (car c)
    c));for convenience but not very type-safe?

(define (cdr! c)
  (if (pair? c)
    (cdr c)
    0));for convenience but not very type-safe?

(define (mul c0 c1);multiplication of a digit
  (cond ((equal? (* (val c0) (val c1) ) 0) 'z)
        ((equal? (* (val c0) (val c1) ) 1) 'p)
        ((equal? (* (val c0) (val c1) ) -1) 'n)))

(define (crsmul c0 c1) ;do cross multiplication
  (crazy2add (mul (car! c0) (car! (cdr! c1))) (mul (car! c1) (car! (cdr! c0)))))

(define (c2mul c0 c1 crry) ;WIP
  (if (and (not (pair? c0)) (not (pair? c1)))
    (crazy2add (mul c0 c1) crry);base case
    (let ([p (crazy2add crry (crsmul c0 c1)) ])
      (cons (car p) (c2mul (cdr! c0) (cdr! c1) p)))))
