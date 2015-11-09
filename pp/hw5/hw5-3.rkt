#lang racket

(require "hw5-1.rkt")
(require "hw5-2.rkt")

(provide is-empty? fst rest length nth-elmt map reduce)

(define unit '())
(define (is-empty? l)
  (equal? l empty))

(define (fst l) ;t list -> t + unit
  ((lambda (l) (case-list (lambda (u) (inr u)) (lambda (h t) (inl h)) l)) l))

(define (rest l)
  ((lambda (l) (case-list (lambda (u) (inr u)) (lambda (h t) (inl t)) l)) l))

(define (length l)
  (if (is-empty? l)
    0
    (+ 1 (length (get-left (rest l))))))

(define (get-left s)
  (case-sum
   (lambda (v) v)
   (lambda (v) 'not-left)
   s))

(define (nth-elmt l i)
  (if (or (< 0 i) (< (length l) i))
    (inr unit) ;unit
    (if (= i 0)
      (fst l)
      (nth-elmt (get-left (rest l)) (sub1 i)))))

(define (map f l) ;apply f to all elements in l
  (if (is-empty? l)
    empty
    (cons (f (get-left (fst l))) (map f (get-left (rest l))))))

(define (reduce l f s)
  (if (is-empty? l)
    s
    (reduce (get-left (rest l)) f (f (get-left (fst l)) s))))



(define l1 empty)
(define l2 (link 1 (link 2 empty)))
