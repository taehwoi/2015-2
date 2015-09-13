#lang racket

(define (gcd n m)
  (fprintf (current-output-port) "this is ~a ~a ~n" n m)
  (if (equal? m 0) (write n)
    (if (equal? n 0) (write m)
      (if (>= n m)
        (gcd (modulo n m) m)
        (gcd n (modulo m n))))))

(gcd 100 25)
