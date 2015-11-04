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
  (if (null? tree)
    0
    (cadr tree)))

(define emptytree '())

(define (sort-by-frequency l)
  (sort l #:key cdr <))
(define (remove-unused frequencies) ;list -> list
  (filter (lambda (p) (> (cdr p) 0)) frequencies))
(define (to-leaf freq) ; to-leaf: (string X int) list -> leaf list
  (map (lambda (l) (leaf (car l) (cdr l))) freq)
  )
#| utility functions |#

(define (vlencode frequencies) ; vlencode: (string X int) list -> (string X (int list)) list
  (if (null? frequencies) ;EXCEPTION
    '()
    (let ()
  (define sorted-freq (sort-by-frequency (remove-unused frequencies))) ;sort the list by frequency
  (define hufftree (hfman sorted-freq)) ;build a huffman tree
  (encode hufftree)) ;assign code to the huffman tree
  ))

;TODO: fix boiler plate code. (or just submit)
(define (hfman freq) ;hfman (string X int) list -> tree
  ;merge the two tree, remove element from list, and go on.
  (define (helper freq treelist) ;helper: (leaf) list * (tree) list -> tree
    (if (null? freq) #| if freq becomes empty, |#
      (if (= (length treelist) 1) #| if treelist's length becomes 1, function terminates |#
        (car treelist) ;BASE CASE
        (let () ;only use tree in the second queue
          (define tree0 (list-ref treelist 0))
          (define tree1 (list-ref treelist 1))
          (define newtree (node tree0 (+ (rootval tree0) (rootval tree1)) tree1))
          (helper freq (append (cddr treelist) (list newtree))))) #| treelist's length decrease by 1. |#
      (if (= (length freq) 1)
        (if (= (length treelist) 0)
          (node (list-ref freq 0) 0 '()) ;dummy tree
          (if (= (length treelist) 1)
            (let () ;(= (length treelist) 1)
              (define leaf0 (list-ref freq 0))
              (define tree0 (list-ref treelist 0))
              (define new-tree-l (append (cdr treelist)(list (node leaf0 (+ (leafval leaf0) (rootval tree0)) tree0))))
              (helper (cdr freq) new-tree-l)) #| freq and treelist's length both decrease by 1 |#
            (let () ;(= (length treelist) 2)
              (define leaf0 (list-ref freq 0))
              (define tree0 (list-ref treelist 0))
              (define tree1 (list-ref treelist 1))
              (if (< (rootval tree1) (leafval leaf0))
                (let () ;use two tree
                  (define newtree (node tree0 (+ (rootval tree0) (rootval tree1)) tree1))
                  (helper freq (append (cddr treelist) (list newtree))))#| freq's length decrease by 2 |#
                (let () ;use one tree and leaf
                  (define newtree (node leaf0 (+ (leafval leaf0) (rootval tree0)) tree0))
                  (helper (cdr freq) (append (cdr treelist) (list newtree))))))))#| freq and treelist's length - 1 |#
        (let ()
          (define leaf0 (list-ref freq 0))
          (define leaf1 (list-ref freq 1))
          (if (null? treelist)
            (let ();only use leafnodes in first queue
              (define newtree (node leaf0 (+ (leafval leaf0) (leafval leaf1)) leaf1))
              (helper (cddr freq) (list newtree)))#| freq's length decrease by 2 |#
            (if (< (leafval leaf1) (rootval (list-ref treelist 0)))
              (let () ;only use leafnodes in first queue
                (define subtree (node leaf0 (+ (leafval leaf0) (leafval leaf1)) leaf1))
                (helper (cddr freq) (append treelist (list subtree))))#| freq's length decrease by 2 |#
              (if (and (< 1 (length treelist)) (< (rootval (list-ref treelist 1)) (leafval leaf0)))
                (let () ;only use tree in the second queue
                  (define tree0 (list-ref treelist 0))
                  (define tree1 (list-ref treelist 1))
                  (define newtree (node tree0 (+ (rootval tree0) (rootval tree1)) tree1))
                  (helper freq (append (cddr treelist) (list newtree))))#| treelist's length decrease by 2 |#
                (let () ;pick one from each queue
                  (define tree0 (list-ref treelist 0))
                  (define newtree (node tree0 (+ (rootval tree0) (leafval leaf0)) leaf0))
                  (helper (cdr freq) (append (cdr treelist) (list newtree)))))))))))#| freq and treelist's length - 1 |#

  #| every time this function is called, either freq,treelist or both length decrease |#
  (helper (to-leaf freq) '()))


(define (encode tree) ;encode: tree -> (string X (int list)) list
  (define (helper tree path) ;helper: tree * (int list) -> (string X (int list)) list
    (if (null? tree) ;BASE CASE: terminates when there are no more subtrees
      '() ;empty list
      (if (isleaf? tree)
        (list (cons (leafstr tree) path)) ;BASE CASE: terminates when the subtree is a leaf
        (append 
          (helper (leftsub tree) (append path (list 0))) ;RECURSION: tree's height gets smaller everytime 
          (helper (rightsub tree) (append path (list 1))))))) ;by visiting right and left children
  (helper tree '()))
