#lang racket

(provide iter)

(define (iter n f); func * int -> func
  (if (= n 0)
    (lambda (x) x) ;identity function
    (lambda (x) (f ((iter (- n 1) f) x)))))
;(f (lambda (x) (iter (- n 1) f) x) )))
    ;((lambda (x) f) (iter (- n 1) f) )))

(define f1 (lambda (x) (* 2 x)))
(define fadd (lambda (x) (cons (+ (car x) 1) (cdr x))))
(define exc (lambda (x) (cons (cdr x) (car x))))

;((iter 2 f1) 2)
(iter 100 f1)
;((lambda (x) (* 3 x)) ((lambda (x) (* 3 x)) 4))
;(f1 ((iter 2 f1)           3))
;((lambda (x) f1) 3)
;(f1 3)
;(f1 (f1 (f1 3)))
;(f1 (f1 (f1 ((lambda (x) x) 3))))
