#lang racket

(define (mymax l)
  (if (null? l) 
    -987654321 ;some random small number
    (max (car l) (mymax (cdr l)))))

(define (myabs n)
  (if (< n 0)
    (* n -1)
    n))


(mymax '(1 6 2 4 3))
(myabs -1)
(myabs 1)
(myabs 0)
