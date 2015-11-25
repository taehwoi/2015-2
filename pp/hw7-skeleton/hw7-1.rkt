#lang racket

(require "sum.rkt")
; Do not assume anything about 'sum'.
; Use the provided functions (inl, inr, case-sum) only.

(provide bstree-make bstree-add-elmt bstree-del-elmt bstree-find-elmt)
(define null 'null)

; tree should look like this:
; ((k,v) (left tree) (right tree))
(define bstree '())

(define (node k v)
  (mcons (cons k v) (mcons null null)))

(define (bstree-make)
    (mcons (cons null null) (mcons null null)))

(define (root bst)
  (mcar bst))
(define (node-key node)
  (car node))
(define (node-val node)
  (cdr node))
(define (left-tree t) ;tree -> tree
  (mcar (mcdr t)))
(define (right-tree t) ;node -> tree
  (mcdr (mcdr t)))

(define (bstree-add-elmt t k v)
  (if (equal? null (node-key (root t)))
    (let ()
      (set-mcar! t (cons k v))
      #f)
      (if (> (car (root t)) k)
        (let ()
          (if (equal? (left-tree t) null)
            (set-mcar! (mcdr t) (node k v))
            (bstree-add-elmt (left-tree t) k v)))
        (let ()
          (if (equal? (right-tree t) null)
            (set-mcdr! (mcdr t) (node k v))
            (bstree-add-elmt (right-tree t) k v))))))

(define (bstree-find-elmt t k)
  (if (equal? null t)
    (inr 'FAIL)
    (if (equal? k (node-key (root t)))
      (inl (node-val (root t)))
      (if (< k (node-key (root t)))
        (bstree-find-elmt (left-tree t) k)
        (bstree-find-elmt (right-tree t) k)))))

(define (bstree-del-elmt t k)
  (if (equal? null t)
    #f
    (if (equal? k (node-key (root t)))
      (inl (node-val (root t)))
      (if (< k (node-key (root t)))
        (bstree-del-elmt (left-tree t) k)
        (bstree-del-elmt (right-tree t) k)))))

(define bst (bstree-make))
bst
(bstree-add-elmt bst 3 "hi")
bst
(bstree-add-elmt bst 2 "gee")
(bstree-add-elmt bst 5 "he")
(bstree-add-elmt bst -1 "she")
bst
