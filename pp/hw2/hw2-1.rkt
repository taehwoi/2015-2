#lang racket

(provide zipper)

(define (zipper l1 l2)
  (cond ((empty? l1) l2)
        ((empty? l2) l1)
        (else (append (list (car l1) (car l2)) (zipper (cdr l1) (cdr l2) )))))
