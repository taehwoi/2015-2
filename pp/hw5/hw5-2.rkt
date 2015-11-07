#lang racket

(provide empty link case-list)

(define empty
  '())

(define (link v l)
  (append (list v) l))

(define (case-list f1 f2 l)
  (if (equal? l empty)
    (f1 l)
    (f2 (car l) (cdr l))))
