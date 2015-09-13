#lang racket

;too slow?
(define (bico n k) ;recursively calculate binary coefficient.
  (cond  ((equal? n k) 1)
         ((equal? k 0) 1)
         (else (+ (bico (- n 1) (- k 1)) (bico (- n 1) k )))))

(define (ptN n k) ;nth row of pascal's triangle,from kth col
    (if (equal? k n)
      "1"
      (string-append (ptN n (+ k 1)) (number->string (bico n k)))))


(define (yanghui n)
  (if (equal? n 1)
    (ptN 0 0)
    (string-append (yanghui (- n 1)) (ptN (- n 1) 0))))

;(ptN 5 0)
(yanghui 4)

;(string-append "1" "2")
