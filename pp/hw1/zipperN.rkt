#lang racket

(define l0 '((1 2 3) (4) (9 10 11 12)))

(define (cdrEach l)
  (if (null? l)
    '()
    (if (null? (cdr (car l))) 
      (cdrEach (cdr l));if becomes empty list, don't add
      (append (list (cdr (car l))) (cdrEach (cdr l)))
    )
  )
)

(define (zip l)
  (if (null? l)
    '()
    (if (null? (car l))
      '()
      (append (list (car (car l))) (zip (cdr l))))))

(define (zipperN l) ;zip,cdrEach ,zip
  (if (null? l) 
    '()
    (append (zip l) (zipperN (cdrEach l)))
  )
)

(zipperN l0)
