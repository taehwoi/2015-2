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
