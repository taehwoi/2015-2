#lang racket

(require "sum.rkt")
; Do not assume anything about 'sum'.
; Use the provided functions (inl, inr, case-sum) only.

(provide bstree-make bstree-add-elmt bstree-del-elmt bstree-find-elmt)
(define null 'null)

; tree should look like this:
; ((k,v) (left tree) (right tree))
(define bstree '())
(define (get-left s)
  (case-sum
   (lambda (v) v)
   (lambda (v) 'not-left)
   s))

(define (bstree-make)
  (set! bstree 
    (mcons '() (mcons (inl null) (inr null)))))

(define (left-child node) ;node -> tree
  (mcar (mcdr node))
  )
(define (right-child node) ;node -> tree
  (mcdr (mcdr node))
  )

(define (bstree-add-elmt t k v)
  'TODO)

(define (bstree-del-elmt t k)
  'TODO)

(define (bstree-find-elmt t k)
  'TODO)

(bstree-make)
bstree
(right-child bstree)
;test -> set bstree's right element to 3
