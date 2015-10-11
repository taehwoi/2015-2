#lang racket

(provide leaf node)
(provide is-empty-tree? is-leaf? leaf-val nth-child)
(define (leaf n)
  (cons  n '()))


(define (node l)
  (if (null? l)
    '()
    (list l))) 

(define (is-empty-tree? t)
  (if (null? t) #t #f))

(define (is-leaf? t) 
  (pair? t))

(define (leaf-val t)
  (car t))

(define (nth-child t n)
  (if (= n 0)
    (if (number? (car t))
      t
      (caar t))
    (nth-child (list (cdar t)) (- n 1))))
