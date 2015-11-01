#lang racket

(provide vlencode)

 
#| utility functions |#
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

(define (sort-by-frequency l)
  (sort l #:key cdr <))
#| utility functions |#

(define freq1 '(("a" . 1) ("b" . 5)))
;TODO 1) make a huffman tree
;     2) make vlencode from that tree (done)

(define (vlencode frequencies) ; vlencode: (string X int) list -> (string X (int list)) list
  'TODO
  (define hufftree (hfman frequencies))
  (encode hufftree))

(define (hfman freq) ;hfman (string X int) list -> tree
  (define l (sort-by-frequency freq)) ;sort the list by frequency
  ;should pick two smallest from the list & current tree.
    ;-> smallest one is ALWAYS chosen, compare second element with tree's rootval
  ;merge the two tree, remove element from list, and go on.
  (define leaf0 (leaf (car (list-ref l 0)) (cdr (list-ref l 0))));smallest one is ALWAYS chosen
  (define leaf1 (leaf (car (cadr l)) (cdr (cadr l))))
  (define tree (node leaf0 (+ (leafval leaf0) (leafval leaf1)) leaf1))
  tree)

(define (encode tree) ;encode: tree -> (string X (int list)) list
  (define (helper tree path) ;helper: tree * (int list) -> (string X (int list)) list
    (if (null? tree)
      '()
      (if (isleaf? tree)
        (list (cons (leafstr tree) path))
        (append 
          (helper (leftsub tree) (append path (list 0)))
          (helper (rightsub tree) (append path (list 1)))))))

  (helper tree '()))
(vlencode freq1)
