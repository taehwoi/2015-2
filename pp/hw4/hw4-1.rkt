#lang racket

(provide vlencode)
(define (leaf str val) ; leaf: string * value -> tree
  (cons 'leaf (list str val)))

(define (node lsub val rsub) ; node: tree * value * tree -> tree
  (cons 'tree (cons val (list lsub rsub))))

(define (isleaf? tree) ; isleaf?: tree -> bool
  (equal? 'leaf (car tree)))

(define (leftsub tree) ; leftsub: tree -> tree
  (car (cddr tree)))

(define (rightsub tree) ; rightsub: tree -> tree
  (cadr (cddr tree)))

(define (leafval tree) ; leafval: tree -> value
  (caddr tree))

(define (leafstr tree) ; leftstr: tree -> string
  (cadr tree))

(define (rootval tree) ; rootval: tree -> value
  (cadr tree))

(define freq0 (list (cons "a" 5) (cons "b" 1) (cons "c" 1) (cons "d" 1)))
(define freq-tiny (list (cons "a" 1) (cons "b" 1)))

;TODO 1) make a huffman tree
;     2) make vlencode from that tree

(define (vlencode frequencies) ; vlencode: (string X int) list -> (string X (int list)) list
  'TODO
  (define hftree (hfman frequencies))
  (encode hftree))

(define (sort-by-frequency l)
  (sort l #:key cdr <))

(define freq1 '(("a" . 1) ("b" . 5)))
(define (hfman freq) ;hfman (string X int) list -> tree
  (define l (sort-by-frequency freq)) ;sort the list by frequency
  (define leaf0 (leaf (car (car l)) (cdr (car l))))
  (define leaf1 (leaf (car (cadr l)) (cdr (cadr l))))
  (define tree (node leaf0 (+ (leafval leaf0) (leafval leaf1)) leaf1))
  tree)
(define mytree2 '(tree 6 (tree 5 (leaf "a" 2) (leaf "b" 3)) (tree 7 (leaf "c" 2) (leaf "d" 5))) )

(define (encode tree) ;encode: tree -> (string X (int list)) list
  (define (helper tree upto) 
    (if (null? tree)
      '()
      (if (isleaf? tree)
        (list (cons (leafstr tree) upto))
        (append 
          (helper (leftsub tree) (append upto (list 0)))
          (helper (rightsub tree) (append upto (list 1)))))))
  (helper tree '()))
(encode mytree2)

(list (cons "a" (list 0 0)) (cons "b" (list 0 1)) (cons "c" (list 1 0)) (cons "d" (list 1 1)))
  

;TODO choose two smallest value and make tree.
