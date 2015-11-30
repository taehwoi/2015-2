#lang racket

'(#t)
'(+ 3 3)

(define (myeval E) ;myeval: (Expression) List -> E
  (define t (car E))
  (cond 
    ((or (number? t) (boolean? t)) t)
    ; ARITHMETIC
    ((equal? t '+) (+ (myeval (list (cadr E))) (myeval (cddr E)))) ;(+ E E)
    ((equal? t '-) (- (myeval (list (cadr E))) (myeval (cddr E))))
    ((equal? t '*) (* (myeval (list (cadr E))) (myeval (cddr E))))
    ((equal? t '=) (= (myeval (list (cadr E))) (myeval (cddr E))))
    ((equal? t '<) (< (myeval (list (cadr E))) (myeval (cddr E))))
    ((equal? t '>) (> (myeval (list (cadr E))) (myeval (cddr E))))

    ; IF
    ((equal? t 'if)
     (if (myeval (cadr E));predicate
       (myeval (list (caddr E))) ;true-action
       (myeval (cdddr E)))) ;false-action

    ; PAIR OPERATIONS
    ((equal? t 'cons) (cons (myeval (cadr E)) (myeval (caddr E))))
    ((equal? t 'car) (car (myeval (cadr E))) ) ;TODO: throw error when (pair? myeval E) = #f
    ((equal? t 'cdr) (cdr (myeval (cadr E))) )
    )) 

(define test '(cdr (cons (+ 3 7) (- 7 3))))
;(cddr test)
(myeval test)
