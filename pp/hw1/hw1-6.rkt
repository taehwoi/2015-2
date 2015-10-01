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

;a tree + operand
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

;use nth-child?
(define (sub-circuit c n); circuit * int -> circuit
  (if (= n 0)
    (cond  ((is-not? c) (leaf-val (cdr c)));not is unary op.
           (else (nth-child (cdr c) 0))); and, or is binary op.
    (nth-child (cdr c) n) ))

(define c1 zero)
(define c2 one)
(define c3 (not-c c1))
(define c4 (and-c c2 c3))
(define c5 (not-c c4))
(define c6 (or-c c3 c5))
;(cdr c3)
;(leaf-val (cdr c3))
;(sub-circuit c3 0)
c6
(sub-circuit c6 0)
(sub-circuit c6 1)
