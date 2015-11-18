#lang racket

;;; If these statements are omitted, your submission will be graded 0.
(provide memo-ways)

(define (memo-ways n m)
  (define table (make-hash))
  (define nil -1)
  (define (ways n m)
    (if (= nil (hash-ref table (cons n m) nil))
      (let ()
        (define val (cond ((= n 0) 1)
                         ((= m 0) 1)
                         (else (+ (ways (- n 1) m)
                                  (ways n (- m 1))))))
        (hash-set! table (cons n m) val)
        (hash-ref table (cons n m)))
      (hash-ref table (cons n m))))

  (ways n m))
