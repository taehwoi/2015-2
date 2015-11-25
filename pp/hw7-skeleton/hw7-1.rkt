#lang racket

(require "sum.rkt")
; Do not assume anything about 'sum'.
; Use the provided functions (inl, inr, case-sum) only.

(provide bstree-make bstree-add-elmt bstree-del-elmt bstree-find-elmt)
(define null 'null)

; tree should look like this:
; ((k,v) (left tree) (right tree))
(define (node k v)
  (mcons (cons k v) (mcons null null)))

(define (bstree-make)
  (node null null))

(define (root bst)
  (if (equal? null bst)
    null
  (mcar bst)))
(define (node-key node)
  (if (equal? null node)
    null
    (car node)))
(define (node-val node)
  (if (equal? null node)
    null
    (cdr node)))
(define (left-tree t) ;tree -> tree
  (mcar (mcdr t)))
(define (right-tree t) ;node -> tree
  (mcdr (mcdr t)))
(define (set-left-tree t lt)
  (set-mcar! (mcdr t) lt))
(define (set-right-tree t rt)
  (set-mcdr! (mcdr t) rt))

(define (set-node t node)
  (set-mcar! t node))

(define (bstree-add-elmt t k v)
  (if (equal? null (node-key (root t)))
    (let ()
      (set-mcar! t (cons k v)) #f)
    (if (equal? k (node-key (root t)))
      (let ()
        (set-mcar! t (cons k v)) #t)
      (if (> (car (root t)) k)
        (let ()
          (if (equal? (left-tree t) null)
            (let ()
              (set-left-tree t (node k v)) #f)
            (bstree-add-elmt (left-tree t) k v)))
        (let ()
          (if (equal? (right-tree t) null)
            (let ()
              (set-right-tree t (node k v)) #f)
            (bstree-add-elmt (right-tree t) k v)))))))

(define (bstree-find-elmt t k)
  (if (equal? null t)
    (inr 'FAIL)
    (if (equal? k (node-key (root t)))
      (inl (node-val (root t)))
      (if (< k (node-key (root t)))
        (bstree-find-elmt (left-tree t) k)
        (bstree-find-elmt (right-tree t) k)))))
(define (is-leaf? t)
  (and (equal? null (left-tree t))
       (equal? null (right-tree t))))

(define (bstree-del-elmt t k)
  (define (left-most-child t)
    (if (equal? null t)
      null
      (if (equal? null (left-tree t))
        t
        (left-most-child (left-tree t)))))

  (if (equal? null (root t)) ;no such item
    #f
    (if (equal? k (node-key (root t))) ;found the item to delete
      (let () (cond  ((is-leaf? t) (set-node t null)) ;if leaf node - just remove node
             ((equal? null (right-tree t)) 
              (let ()
                (set-node t (root (left-tree t)))
                (set-right-tree t (right-tree (left-tree t)))
                (set-left-tree t (left-tree (left-tree t)))))
             ((equal? null (left-tree t)) ;can merge with else, but keep it to make the tree pretty
              (let ()
                (set-node t (root (right-tree t)))
                (set-left-tree t (left-tree (right-tree t)))
                (set-right-tree t (right-tree (right-tree t)))))
             (else 
               (let ()
                 (define new-node (left-most-child (right-tree t)))
                 (define tmp (root new-node)) ;new-node will be removed after next line
                 (bstree-del-elmt t (node-key tmp))
                 (set-node t tmp) ;do this after deletion to avoid infinite loops
                 ))) #t)
      (if (< k (node-key (root t))) ;go down the tree
        (bstree-del-elmt (left-tree t) k)
        (bstree-del-elmt (right-tree t) k)))))
