#lang racket


; We auto-grade only "react" function; other functions are not
; auto-graded. However, S, K, I, v, and a are required for
; grading. See hw4-2-grade.rkt for more information.
(provide react S K I v a)


; Implement react. 
;
; In the document, react has the type solution -> void.
; However, implement react: solution -> string for ease of grading.
; Return the string of the given solution printed by pprint.
;
; See hw4-2-grade.rkt for more information on what the returned string should look like.
; In short,
; S prints "S";
; K prints "K";
; I prints "I";
; variable x prints "x";
; tuple (E F) prints "(" + E + " " + F + ")".

;(define (react e) ; execute: solution -> string
  ;(pprint 
   ;'TODO
   ;))

(define (s-react e) ; solution -> string
  (list (append (cdaar e) (cdr e)) (append (cdar e) (cdr e))))

;if no change return e?
(define (react e)
  (if (null? (cdr e))
    e
    (if (isI? e) ;check I? 1st
      (react (car (cdr e)))
      (if (isK? e)
        (react (cdr (car e)))
        (if (isS? e)
          (react (s-react e))
          e
          )))))

(define S ; S: solution
  'S)
(define K ; K: solution
  'K)
(define I 'I)
(define (v str) ; v: string -> solution
  str)
(define (a lhs rhs) ; a: solution * solution -> solution
  (list lhs rhs))
; You may need the following tree interface.

(define (isI? e) ; isI?: solution -> bool
  (if (null? e)
    #f
    (eq? (car e) 'I)))
(define (isK? e) ; isK?: solution -> bool
  (if (null? e)
    #f
    (eq? (caar e) 'K)))
(define (isS? e) ; isS?: solution -> bool
  (if (null? e)
    #f
    (eq? (caaar e) 'S)))

(define (isv? e) ; isv?: solution -> bool
  'TODO)
(define (isa? e) ; isa?: solution -> bool
  'TODO)
(define (var e) ; var: solution -> string
  'TODO)
(define (al e) ; al: solution -> solution
  'TODO)
(define (ar e) ; ar: solution -> solution
  'TODO)
(define (pprint e) ; pprint: solution -> string
  'TODO)

;(react (a (a (a S (v "x")) (v "y")) (v "z")))
 
(react (a (a (a (a S (a K (a S I))) K) (v "x")) (v "y")))
(react (a (a (a (a (a I I) S) I) (a K K)) (a (a (a I (v "doo0")) K) (a I (a S I)))))
