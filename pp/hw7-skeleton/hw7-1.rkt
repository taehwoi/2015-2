#lang racket

(require "sum.rkt")
; Do not assume anything about 'sum'.
; Use the provided functions (inl, inr, case-sum) only.

(provide bstree-make bstree-add-elmt bstree-del-elmt bstree-find-elmt)
(define null 'null)

; tree should look like this:
; ((k,v) (left tree) (right tree))
(define (make-node k v)
  (cons k v))

(define (make-a-tree k v)
  (mcons (make-node k v) (mcons null null)))

(define (bstree-make)
  (make-a-tree null null))

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

(define (set-left-tree! t lt)
  (set-mcar! (mcdr t) lt))
(define (set-right-tree! t rt)
  (set-mcdr! (mcdr t) rt))
(define (set-node! t node)
  (set-mcar! t node))

(define (is-leaf? t)
  (and (equal? null (node-key (left-tree t)))
       (equal? null (node-key (right-tree t)))))

(define (bstree-add-elmt t k v)
  (cond
    ((equal? null (node-key t)) (let () (set-node! t (make-node k v)) #f))
    ((equal? k (node-key t)) (let () (set-node! t (make-node k v)) #t))
    ((< k (node-key t))
      (let ()
        (if (equal? (left-tree t) null)
          (let () (set-left-tree! t (make-a-tree k v)) #f)
          (bstree-add-elmt (left-tree t) k v))))
    (else 
      (let ()
        (if (equal? (right-tree t) null)
          (let ()
            (set-right-tree! t (make-a-tree k v)) #f)
          (bstree-add-elmt (right-tree t) k v))))))

(define (bstree-find-elmt t k)
  (cond  
    ((equal? null (node-key t)) (inr 'FAIL))
    ((equal? k (node-key t)) (inl (node-val t)))
    ((< k (node-key t)) (bstree-find-elmt (left-tree t) k))
    (else (bstree-find-elmt (right-tree t) k))))

(define (get-min t)
  (cond  
    ((equal? null t) null)
    ((equal? null (node-key (left-tree t))) t)
    (else (get-min (left-tree t)))))

(define (bstree-del-elmt t k)
  (cond
    ((equal? null (node-key t)) #f)
    ((equal? k (node-key t)) 
      (let () 
        (cond  
          ((is-leaf? t) (set-node! t null)) ;if leaf tree - just remove tree
          ((equal? null (right-tree t))
           (let ()
             (set-node! t (node (left-tree t)))
             (set-right-tree! t (right-tree (left-tree t)))
             (set-left-tree! t (left-tree (left-tree t)))))
          ((equal? null (left-tree t)) ;can merge with else, but keep it to make the tree pretty
           (let ()
             (set-node! t (node (right-tree t)))
             (set-left-tree! t (left-tree (right-tree t)))
             (set-right-tree! t (right-tree (right-tree t)))))
          (else 
            (let ()
              (define min-tree (get-min (right-tree t)))
              (set-node! t (node min-tree))
              (bstree-del-elmt (right-tree t) (node-key min-tree))
              ))) #t))
    ((< k (node-key t)) (bstree-del-elmt (left-tree t) k))
    ((< k (node-key t)) (bstree-del-elmt (left-tree t) k))
    (else (bstree-del-elmt (right-tree t) k))))
