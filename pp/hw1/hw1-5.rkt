#lang racket
(require "hw1-4.rkt")
(provide model make-branch make-mobile)
(provide weight is-balanced?)


(define (model n)
  (cons 'model n))

(define (make-branch n m)
  (cons 'branch (cons n m)))

(define (make-mobile b1 b2)
  (list b1 b2))

(define (weight m)
  (cond  ((eq? 'model (car m)) (cdr m))
         ((eq? 'branch (car m)) (weight (cdr (cdr m))))
         ;a 2-branched tree
         ((+ (weight (car m)) (weight (car (cdr m)))))
       ))

(define (torque b)
    (* (car (cdr b)) (weight b)))

(define (is-balanced? m)
  (if (eq? 'model (car m)) 
    #t;a model is balanced
    (if (= (torque (car m)) (torque (car (cdr m))))
      (and 
        (is-balanced? (cdr (cdr (car m)))) 
        (is-balanced? (cdr (cdr (car (cdr m))))))
      #f)));don't have to look deeper 

