#lang racket
(provide leaf node)
(provide is-empty-tree? is-leaf? leaf-val nth-child)

(define (leaf n)
  (list 'leaf n))

(define (node l)
  (cons 'node l))

(define (is-empty-tree? t)
  (if (null? (cdr t))
      #t
      #f))

(define (is-leaf? t)
  (if (equal? (car t) 'leaf)
      #t
      #f))

(define (leaf-val t)
  (cadr t))

(define (nth-child t n)
  (if (= n -1)
      (car t)
      (nth-child (cdr t) (- n 1))))