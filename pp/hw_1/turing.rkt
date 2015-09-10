#lang racket

(define (t n)
  (if (equal? n 0) "0"
    (string-append (t (- n 1)) "1")))

(define (t2 n)
  (if (equal? n 0) ""
    (string-append (t2 (- n 1)) (t n))))

(printf (t2 3))
