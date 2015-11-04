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
  (equal? 'S e))
(define (isK? e) ; isS?: solution -> bool
  (equal? 'K e))
(define (isI? e) ; isS?: solution -> bool
  (equal? 'I e))
(define (isv? e) ; isv?: solution -> bool
  (if (or (null? e) (not (list? e)))
    #f
    (equal? 'v (car e))))
(define (isa? e) ; isa?: solution -> bool
  (if (or (null? e) (not (list? e)))
    #f
    (equal? (car e) 'a)))
(define (var e) ; var: solution -> string
  (cadr e))

(define (al e) ; al: solution -> solution
  (if (not (isa? e))
    '()
    (cadr e)))

(define (ar e) ; ar: solution -> solution
  (if (not (isa? e))
    '()
    (caddr e)))

;(define SII (a (a (a S I) I) (a (a S I) I)))
(define (pprint e) ; pprint: solution -> string
  (cond  ((isS? e) "S")
         ((isK? e) "K")
         ((isI? e) "I")
         ((isa? e) (string-append"("(pprint (al e))" "(pprint (ar e))")"))
         ((isv? e) (cadr e))
         ))

(define (react e)
  #| util func. |#
  (define (react-type e) ; react-type: solution -> symbol
    (cond 
      ((equal? (al (al (al e))) 'S) 'S)
      ((equal? (al (al e)) 'K) 'K)
      ((equal? (al e) 'I) 'I)
      (else 'NO))) ;can't react
  (define (S-react e) ; solution -> solution
    (a (a (ar (al (al e))) (ar e)) (a (ar (al e)) (ar e))))
  (define (K-react e); solution -> solution
    (ar (al e)))
  (define (I-react e); solution -> solution
    (ar e))
  #| end of util func. |#

  (define (react-helper e) 
    (define rule (react-type e))
    (cond 
      ((equal? rule 'S) (S-react e))
      ((equal? rule 'K) (K-react e))
      ((equal? rule 'I) (I-react e))
      ((isa? e) (a (react-helper (al e)) (react-helper (ar e)))) ;check this last to avoid errors.
      (else e)))
  (if (equal? e (react-helper e)) 
    (pprint e);can't react more
    (react (react-helper e)))) ;keep going on
