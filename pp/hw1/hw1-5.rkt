#lang racket
(require "hw1-4.rkt")
(provide model make-branch make-mobile)
(provide weight is-balanced?)

(define (model n)
  (leaf n))

(define (make-branch n m)
  (cons 'branch (cons n m)))

(define (is-branch? b)
  (eq? 'branch (car b)))

(define (make-mobile b1 b2)
  (node (list b1 b2)))

(define (weight m)
  (cond  ((is-leaf? m) (leaf-val m))
         ((is-branch? m) (weight (cdr (cdr m))))
         ;is a bin-tree
         ((+ (weight (car (cdr m))) (weight (car (cdr (cdr m)))) ))))

(define (torque b) ;branch -> int
    (* (car (cdr b)) (weight b)))

(define (is-balanced? m)
  (if (is-leaf? m)
    #t;a leaf is balanced
    (if (= (torque (car (cdr m))) (torque (car (cdr (cdr m)))))
      (and 
        (is-balanced? (cdr (cdr (car (cdr m))))) 
        (is-balanced? (cdr (cdr (car (cdr (cdr m)))))))
      #f)));don't have to look deeper 

