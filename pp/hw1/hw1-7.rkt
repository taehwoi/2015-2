#lang racket
(require "hw1-4.rkt")
(require "hw1-6.rkt")
(provide output)

(define (action c b0 b1) ;circuit -> procedure
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
