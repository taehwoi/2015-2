#lang racket
(require "hw1-4.rkt")
(provide zero one not-c and-c or-c)
(provide is-zero? is-one? is-not? is-and? is-or? sub-circuit)

(define zero
  #f)
(define one
  #t)
(define (not-c c)
  (cons 'not (list c)))
(define (and-c c1 c2)
  (cons 'and (list c1 c2)))

(define (or-c c1 c2)
  (cons 'or (list c1 c2)))

(define (is-zero? c)
  (if (not (pair? c));0 or 1
    (not c)
    #f))
(define (is-one? c)
  (if (not (list? c));0 or 1
    c
    #f))

(define (is-not? c)
  (eq? (car c) 'not))
(define (is-and? c)
  (eq? (car c) 'and))
(define (is-or? c)
  (eq? (car c) 'or))

(define (sub-circuit c n)
  (if (= n 0)
    (car (cdr c))
    (sub-circuit (cdr c) (- n 1)) ))
