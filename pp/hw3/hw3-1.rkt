#lang racket
; not for execution
; write a type of each expression after the closing parenthesis

(define (sigma lower #|int|# upper #|int|# ) ; -> 
  (lambda (f) #|proc|# ; int *
    (define (loop n)
      (if (> n upper)
          1
          (+ (f n) (loop (+ n 1)))))
    (loop lower)
    )
  )

(define (generic-sum lower upper f larger base op inc)
  (if (larger lower upper) base
      (op (f lower)
          (generic-sum (inc lower) upper f larger base op inc))
      )
  )

(define (map #|(X->Y)*Xlist->Y list|# f #|proc(X->Y)|# l #|list|#); (X -> Y) * X list -> Y list
  (if (null? #|bool|# l #|list|# ) '() #|list|#
      (cons (f #|X->Y|# (car l)#|element(X)|#)#|Y|# (map f #|(X->Y)|#(cdr l)#|Xlist|#)#|Ylist|#))) #|list|#

(define (reduce l #|list|# op #|(X*Y->Y)|# init #|Y|#)
  (if (null? #|proc(list->bool)|# l #|list|#)#|bool|# init #|Y|#
      (op #|proc(X*Y->Y)|# (car l)#|X|# (reduce (cdr l) op init)#|Y|#) #|Y|#
      )
  )

(define (map-reduce #|((X->Y)*list)|# f #|(X->Y)|# l op init #|T|#)
  (reduce (map f l) op init)
  )

;(reduce '(1 2 3) * 1)
;null?
(map-reduce even? '(1 2 3 4) xor #t)
;(map even? '(1 2 3 4))
