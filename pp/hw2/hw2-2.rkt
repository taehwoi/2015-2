#lang racket

(provide zipperN)

(define (forEach ll op) ;listOflist * operation -> listOflist
  (map (lambda (l)
         (if (null? l)
           '()
           (op l))) ll))

(define (zip l);list of list -> list
  (if (andmap null? l)
    '()
    (if (null? (car l))
      (zip (cdr l))
      (append (list (car (car l))) (zip (cdr l))))))

(define (zipperN l) ;zip,cdrEach ,zip
  (if (andmap null? l)
    '()
    (append (zip l) (zipperN (forEach l cdr)))))
(zipperN '((1 3 5 7) (2 8) () (10 20 30)))
