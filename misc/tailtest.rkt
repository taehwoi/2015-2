#lang racket

(define (sumupto n);add up to n without tails recursion
  (if (= n 0)
    0
    (+ n (sumupto (- n 1)))))

(define (tailsum n aux)
  (if (= n 0)
     aux
     (tailsum (- n 1) (+ aux n))))