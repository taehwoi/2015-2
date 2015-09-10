#lang racket

(define (zip l1 l2)
  (cond ((empty? l1) l2)
        ((empty? l2) l1)
        (else (append (list (car l1) (car l2)) (zip (cdr l1) (cdr l2) )))))

;(write (list (car '(1 2 3 4)) (car ' (5 6)) ))
;(write (cons (car '(1 2 3 4)) (car '(5 6))))
(write (zip '(1 2 3 4) '(5 6)))
