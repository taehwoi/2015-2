#lang racket

(define (list-even? l)
  (if (null? l)
    '()
    (cons (if (even? (car l)) #t #f) 
          (list-even? (cdr l)))))

(define (l-even? l)
  (if (even? l) #t #f))

(define (my-map f l)
  (if (null? l)
    '()
    (cons (f (car l)) (my-map f (cdr l)))))

(define (my-filter t l)
  (if (null? l)
    '()
    (if (t (car l)) 
      (cons (car l) (my-filter t (cdr l)))
    (my-filter t (cdr l)))))

;(list-even? '(1 2 3 4 5 6))

(define (listeven? l)
  (my-map even? l))

(define (or-multi boolL)
  (if (null? boolL)
    #f
    (or (car boolL) (or-multi (cdr boolL)))))


(define (has-even? l)
  (if (null? (filter even? l))
    #f
    #t ))

(define (new-has-even? l)
  (or-multi (listeven? l)))

(has-even? '( 1 3 ))
(new-has-even? '(1 2))
