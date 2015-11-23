#lang racket

'(#t)
'(+ 3 3)

(define (myeval E) ;myeval: (Expression) List -> E
  (define t (car E))
  (cond 
    ((or (number? t) (boolean? t)) t)
    ((equal? t '+) (+ (myeval (cdr E)) (myeval (cddr E)))) ;(+ E E)
    ((equal? t '-) -)
    ((equal? t '*) *)
    ((equal? t '=) =)
    ((equal? t '<) <)
    ((equal? t '>) >)
    )) 

(myeval '(+ 3 3))

