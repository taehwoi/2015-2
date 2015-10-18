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

(define (and-c c1 c2); operand * tree -> circuit
  (cons 'and (node (list c1 c2))))

(define (or-c c1 c2)
  (cons 'or (node (list c1 c2))))

(define (is-zero? c)
  (if (is-leaf? c)
    (not (leaf-val c))
    #f))

(define (is-one? c)
  (if (is-leaf? c)
    (leaf-val c)
    #f))

(define (is-not? c)
  (eq? (car c) 'not))
(define (is-and? c)
  (eq? (car c) 'and))
(define (is-or? c)
  (eq? (car c) 'or))

(define (sub-circuit c n); circuit * int -> circuit
  (if (= n 0)
    (cond  ((is-not? c) (leaf-val (cdr c)));not is unary op.
           (else (nth-child (cdr c) 0))); and, or is binary op.
    (nth-child (cdr c) n) ))
