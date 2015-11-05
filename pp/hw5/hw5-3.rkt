#lang racket

(require "hw5-1.rkt")
(require "hw5-2.rkt")

(provide is-empty? fst rest length nth-elmt map reduce)

(define (is-empty? l)
  (equal? l empty))

(define (fst l)
  (define safe-car
    (lambda (l)
      (case-list
        (lambda (u) (cons u empty))
        (lambda (h t) h)
        l)))
  (safe-car l))

(define (rest l)
  (define safe-cdr
    (lambda (l)
      (case-list
        (lambda (u) empty)
        (lambda (h t) t)
        l)))
  (safe-cdr l))

(define (length l)
  'TODO)

(define (nth-elmt l i)
  'TODO)

(define (map f l)
  'TODO)

(define (reduce l f s)
  'TODO)

(define unit '())

(define (right-unit? s)
  (case-sum
   (lambda (v) #f)
   (lambda (v) (equal? v '()))
   s))
(define l1 empty)
(define l2 (link 1 (link 2 empty)))
;(equal? (right-unit? (fst l1)) #t)
(fst l1)
