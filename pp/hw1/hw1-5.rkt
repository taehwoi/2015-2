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

(define (make-mobile b1 b2) ;branch * branch -> tree
  (node (list b1 b2)))

(define (weight m)
  (cond  ((is-leaf? m) (leaf-val m))
         ((is-branch? m) (weight (cdr (cdr m))))
         ;is a bin-tree
         ((+ (weight (nth-child m 0)) (weight (nth-child m 1)) ))))

(define (torque b);branch -> int
    (* (car (cdr b)) (weight b)))

(define (is-balanced? m);tree -> bool
  (if (is-leaf? m)
    #t;a leaf is balanced
    (if ( = (torque (car (cdr m))) (torque (car (cdr (cdr m)))) )
      (and ;if balanced at top, look deeper
        (is-balanced? (cdr (cdr (nth-child m 0))))
        (is-balanced? (cdr (cdr (nth-child m 1)))))
      #f)));don't have to look deeper 

(define mdl1 (model 1))
(define mdl2 (model 2))
(define b1 (make-branch 12 mdl1))
(define b2 (make-branch 7 mdl2))
(define mbl1 (make-mobile b1 b2))

(define mdl3 (model 6))
(define mdl4 (model 4))
(define b3 (make-branch 10 mdl3))
(define b4 (make-branch 15 mdl4))
(define mbl2 (make-mobile b4 b3))

(define b5 (make-branch 10 mbl1))
(define b6 (make-branch 3 mbl2))
(define mbl3 (make-mobile b5 b6))
(weight mbl3)
