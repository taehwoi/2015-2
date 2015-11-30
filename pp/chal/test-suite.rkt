#lang racket
(require "challenge.rkt")

(define (test a b)
  (if (equal? a b)
    (printf "O\n")
    (let ()
      (printf "X\n  yours: ")
      (write a)
      (newline)
      (printf "  ans: ")
      (write b)
      (newline)
      )))
;to use eval
(define ns (make-base-namespace))

(define t0 '3)
(test 
  (myeval t0 '())
  (eval t0 ns))

(define t1 '#t)
(test 
  (myeval t1 '())
  (eval t1 ns))

(define t2 ''())
(test 
  (myeval t2 '())
  (eval t2 ns))

(define t3 '(+ 1 2))
(test 
  (myeval t3 '())
  (eval t3 ns))

(define t4 '(+ (* 3 5) (- 6 1)))
(test 
  (myeval t4 '())
  (eval t4 ns))

(define t5 '(if #f (+ 1 2) (* 3 5)))
(test 
  (myeval t5 '())
  (eval t5 ns))

(define t6 '(cons 1 2))
(test 
  (myeval t6 '())
  (eval t6 ns))

(define t7 '(cons (cons 1 2) (cons 3 4)))
(test 
  (myeval t7 '())
  (eval t7 ns))

(define t8 '(cons (+ 1 2) (* 3 4)))
(test 
  (myeval t8 '())
  (eval t8 ns))

(define t9 '(car (cons (+ 1 2) (* 3 4))))
(test 
  (myeval t9 '())
  (eval t9 ns))

(define t10  '(let ((p (cons 1 (cons 2 '())))) (cons 0 p)))
(test 
  (myeval t10 '())
  (eval t10 ns))
