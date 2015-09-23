#lang racket

;(letrec ((foo (lambda (x) (+ x 1)))) (foo 3))
;(let x (+ x 1) x)
;(let ([x 5]) x)

;(let ((x 3)) ((let ((x (+ x 1))) x)))
;(let ((x 1)) (let ((x (+ x 2))) x))
;(letrec ((f (lambda (x) (* f 2)))) (f 3))

(define myfac (lambda (x) 
                (letrec ((fac (lambda (n)
                (if (<= n 0)
                  1
                  (* n (fac (- n 1)))))))(fac x))
                ))

(myfac 3)
(define fac
    (lambda (n) (if (= n 0)
                  1
                  (* n (fac (- n 1))))))

(fac 6)
;
;((lambda (x) (+ x 1)) 3)
;((lambda (x) (+ x 1)) 3)

;(letrec ((foo 3)) foo)
;(letrec ((x (+ x 1))) (x 1))
