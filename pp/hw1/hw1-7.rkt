#lang racket
(require "hw1-6.rkt")
(provide output)

(define (action o b0 b1) ;operation -> function
  (cond  ((eq? o 'not) (not b0))
         ((eq? o 'and) (and b0 b1))
         ((eq? o 'or) (or b0 b1))))

(define (myaction o) ;operation -> function
  (cond  ((eq? o 'not) not)
         ((eq? o 'and) (and))
         ((eq? o 'ad) min)
         ((eq? o 'or) xor)))

((myaction 'or) #t #t)

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
;(boolval c3)
