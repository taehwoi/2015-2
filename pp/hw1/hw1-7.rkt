#lang racket
(require "hw1-6.rkt")
(provide output)

(define (myand o1 o2)
    (and o1 o2))

(define (myor o1 o2)
    (or o1 o2))

(define (action o b0 b1) ;operation -> function
  (cond  ((eq? o 'and) (and b0 b1))
         ((eq? o 'or) (or b0 b1))))

(define (output c)
  (if (boolval c)
    1
    0))

(define (boolval c)
  (if (boolean? c)
    c
    (if (eq? (car c) 'not)
      (not (boolval (sub-circuit c 0)))
      (action (car c) (boolval (sub-circuit c 0)) (boolval (sub-circuit c 1))))))


(define c1 zero)
(define c2 one)
(define c3 (not-c c1))
(define c4 (and-c c2 c3))
(define c5 (not-c c4))
(define c6 (or-c c3 c5))
;c3
;(output c3)
(boolval c6)
