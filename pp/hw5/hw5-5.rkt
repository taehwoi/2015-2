#lang racket

;;; If these statements are omitted, your submission will be graded 0.

(provide equal)
(provide size)
(provide beautiful)


; You can use the definitions and functions defined in hw5-4.rkt:
; black, white, glue-array-from-tree, glue-array-from-array,
; rotate-array, neighbor-array, pprint-array, is-array?,
; glue-tree-from-tree, glue-tree-from-array, rotate-tree,
; neighbor-tree, pprint-tree, is-tree?, array-to-tree, tree-to-array,
; glue, rotate, neighbor, pprint
(require "hw5-4.rkt")



;;; interfaces
(define (equal f g) ; equal: form * form -> form
  (equal? (pprint f) (pprint g))) ;for ease, get two strings and compare them
(equal black white)


#;(define (size f) ; size: form -> int
  (if (not (list? f))
    0 ;one tile
    (if (is-array? f) 
      (/ (log (length (cdr f))) (log 2)) ; width = height #| to integer? |#
      (size (tree-to-array f)))))

; tree: size(F) = 1 + size(subF) 
(define (size f) ; form -> int
  (define (helper f) ;(tree)form -> int
    (if (not (list? f))
      0 ;one tile
      (+ 1 (helper (car f))))); subtree

  (if (not (list? f))
    0 ;one tile
    (if (is-array? f) 
      (helper (cdr (array-to-tree f))) ;cdr to remove tag
      (helper (cdr f)))))

(define (beautiful f) ; beautiful: form -> bool
  (define (beautiful-sym f)
    'TODO)
  (define (beautiful-neighbor f)
    'TODO)
  (or (beautiful-sym f) (beautiful-neighbor f)))
