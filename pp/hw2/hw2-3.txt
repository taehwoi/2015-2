#lang racket

(provide iter)

(define (iter n f); func * int -> func
  (if (= n 0)
    (lambda (x) x) ;identity function
    (lambda (x) (f ((iter (- (abs n) 1) f) x)))))
