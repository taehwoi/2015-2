#lang racket
(provide leaf node)
(provide is-empty-tree? is-leaf? leaf-val nth-child)

(define (leaf n);leaf-tree
  (cons 'leaf n))

(define (node l)
  (cons 'tree l))

(define (is-empty-tree? t)
  (null? (cdr t)))

(define (is-leaf? t)
  (equal? 'leaf (car t)))

(define (is-tree t)
  (equal? 'tree (car t)))

(define (leaf-val t)
  (cdr t))

(define (nth-child t n)
  (if (= n 0)
    (car (cdr t))
    (nth-child (cdr t) (- n 1))))
