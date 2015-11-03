#lang racket

(provide react S K I v a)

(define S ; S: solution
  'S)
(define K ; K: solution
  'K)
(define I ; I: solution
  'I)
(define (v str) ; v: string -> solution
  (list 'v str))
(define (a lhs rhs) ; a: solution * solution -> solution
  (list 'a lhs rhs))

(define (isS? e) ; isS?: solution -> bool
  (eq? 'S e))
(define (isK? e) ; isS?: solution -> bool
  (eq? 'K e))
(define (isI? e) ; isS?: solution -> bool
  (eq? 'I e))
(define (isv? e) ; isv?: solution -> bool
  (if (or (null? e) (not (list? e)))
    #f
    (eq? 'v (car e))))
(define (isa? e) ; isa?: solution -> bool
  (if (or (null? e) (not (list? e)))
    #f
    (eq? (car e) 'a)))
(define (var e) ; var: solution -> string
  (cadr e))

(define (al e) ; al: solution -> solution
  (if (or (not (list? e)) (null? e))
    '()
    (cadr e)))

(define (ar e) ; ar: solution -> solution
  (if (or (not (list? e)) (null? e))
    '()
    (caddr e)))

(define (pprint e) ; pprint: solution -> string
  (cond  ((isS? e) "S")
         ((isK? e) "K")
         ((isI? e) "I")
         ((isa? e) (string-append "(" (pprint (al e)) " " (pprint (ar e))  ")"))
         ((isv? e) (cadr e))))

(define (s-react e)
  (a (a (ar (al (al e))) (ar e)) (a (ar (al e)) (ar e))))
; In short,
; S prints "S";
; K prints "K";
; I prints "I";
; variable x prints "x";
; tuple (E F) prints "(" + E + " " + F + ")".

(define (react e)
  (pprint (cover e)))

(define (cover e)
  (write e)
  (newline)
  (define (react-helper e) ; execute: solution -> string
    (cond 
      ((eq? (al e) 'I) (ar e))
      ((eq? (al (al e)) 'K) (ar (al e)))
      ((eq? (al (al e)) 'K) (ar (al e)))
      ((eq? (al (al (al e))) 'S) (s-react e))
      ((isa? e) (a (react-helper (al e)) (react-helper (ar e))))
      (else e)))

  (if (eq? e (react-helper e))
    e
    (react-helper (react-helper e))))
(react (a (a (a (v "x") (v "y")) (v "z")) (v "w")))
