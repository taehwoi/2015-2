#lang racket
(require "hw1-4.rkt")
(require "hw1-6.rkt")
(provide output)

(define (action c b0 b1) ;curcuit -> procedure
  (cond  ((is-and? c) (and b0 b1))
         ((is-or? c) (or b0 b1))))

(define (output c)
  (if (boolval c)
    1
    0))

(define (boolval c)
  (if (is-leaf? c)
    (leaf-val c)
    (if (is-not? c)
      (not (boolval (sub-circuit c 0)))
      (action c (boolval (sub-circuit c 0)) (boolval (sub-circuit c 1))))))


(define c1 zero)
(define c2 one)
(define c3 (not-c c1))
(define c4 (and-c c2 c3))
(define c5 (not-c c4))
(define c6 (or-c c3 c5))
