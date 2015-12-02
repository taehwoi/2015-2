#lang racket

(require "sum.rkt")
; Do not assume anything about 'sum'.
; Use the provided functions (inl, inr, case-sum) only.

(provide bstree-make bstree-add-elmt bstree-del-elmt bstree-find-elmt)
(define null 'null)

; tree should look like this:
; ((k,v) (left tree) (right tree))
(define (make-node k v)
  (mcons (cons k v) (mcons null null)))

(define (bstree-make)
  (make-node null null))

(define (node bst)
  (if (equal? null bst)
    null
    (mcar bst)))
(define (node-key t)
  (if (equal? null (node t))
    null
    (car (node t))))
(define (node-val t)
  (if (equal? null (node t))
    null
    (cdr (node t))))
(define (left-tree t) ;tree -> tree
  (if (equal? t null)
    null
    (mcar (mcdr t))))
(define (right-tree t) ;make-node -> tree
  (if (equal? t null)
    null
    (mcdr (mcdr t))))
(define (set-left-tree t lt)
  (set-mcar! (mcdr t) lt))
(define (set-right-tree t rt)
  (set-mcdr! (mcdr t) rt))

(define (set-node t make-node)
  (set-mcar! t make-node))

(define (bstree-add-elmt t k v)
  (if (equal? null (node-key t))
    (let ()
      (set-mcar! t (cons k v)) #f)
    (if (equal? k (node-key t))
      (let ()
        (set-mcar! t (cons k v)) #t)
      (if (> (node-key t) k)
        (let ()
          (if (equal? (left-tree t) null)
            (let ()
              (set-left-tree t (make-node k v)) #f)
            (bstree-add-elmt (left-tree t) k v)))
        (let ()
          (if (equal? (right-tree t) null)
            (let ()
              (set-right-tree t (make-node k v)) #f)
            (bstree-add-elmt (right-tree t) k v)))))))

(define (bstree-find-elmt t k)
  (if (equal? null (node-key t))
    (inr 'FAIL)
    (if (equal? k (node-key t))
      (inl (node-val t))
      (if (< k (node-key t))
        (bstree-find-elmt (left-tree t) k)
        (bstree-find-elmt (right-tree t) k)))))

(define (is-leaf? t)
  (and (equal? null (node-key (left-tree t)))
       (equal? null (node-key (right-tree t)))))

(define (get-min t)
  (if (equal? null t)
    null
    (if (equal? null (node-key (left-tree t)))
      t
      (get-min (left-tree t)))))

(define (bstree-del-elmt t k)
  (if (equal? null (node-key t)) ;no such item
    #f
    (if (equal? k (node-key t)) ;found the item to delete
      (let () 
        (cond  ((is-leaf? t) (set-node t null)) ;if leaf make-node - just remove make-node
               ((equal? null (right-tree t))
                (let ()
                  (set-node t (node (left-tree t)))
                  (set-right-tree t (right-tree (left-tree t)))
                  (set-left-tree t (left-tree (left-tree t)))))
               ((equal? null (left-tree t)) ;can merge with else, but keep it to make the tree pretty
                (let ()
                  (set-node t (node (right-tree t)))
                  (set-left-tree t (left-tree (right-tree t)))
                  (set-right-tree t (right-tree (right-tree t)))))
               (else 
                 (let ()
                   (define min-tree (get-min (right-tree t)))
                   (set-node t (node min-tree))
                   (bstree-del-elmt (right-tree t) (node-key min-tree))
                   ))) #t)
      (if (< k (node-key t)) ;go down the tree
        (bstree-del-elmt (left-tree t) k)
        (bstree-del-elmt (right-tree t) k)))))
