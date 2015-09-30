#lang racket
(require "hw1-4.rkt")
(provide zero one not-c and-c or-c)
(provide is-zero? is-one? is-not? is-and? is-or? sub-circuit)

(define zero
  (leaf #f))
(define one
  (leaf #t))

(define (not-c c)
  (cons 'not (leaf c)))

(define (and-c c1 c2)
  (cons 'and (node (list c1 c2))))

(define (or-c c1 c2)
  (cons 'or (node (list c1 c2))))

(define (is-zero? c)
  (not (leaf-val c)))

(define (is-one? c)
  (leaf-val c))

(define (is-not? c)
  (eq? (car c) 'not))
(define (is-and? c)
  (eq? (car c) 'and))
(define (is-or? c)
  (eq? (car c) 'or))

(define (sub-circuit c n)
  (if (= n 0)
    (cond  ((is-not? c) (cdr (cdr c)))
           (else (car (cdr (cdr c)))))
    (sub-circuit (cdr c) (- n 1)) ))

(define c1 zero)
(define c2 one)
(define c3 (not-c c1))
(define c4 (and-c c2 c3))
(define c5 (not-c c4))
(define c6 (or-c c3 c5))
