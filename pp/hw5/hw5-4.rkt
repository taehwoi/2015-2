#lang racket

(provide black)
(provide white)

(provide glue-array-from-tree)
(provide glue-array-from-array)
(provide rotate-array)
(provide neighbor-array)
(provide pprint-array)
(provide is-array?)

(provide glue-tree-from-tree)
(provide glue-tree-from-array)
(provide rotate-tree)
(provide neighbor-tree)
(provide pprint-tree)
(provide is-tree?)

(provide array-to-tree)
(provide tree-to-array)

(provide glue)
(provide rotate)
(provide neighbor)
(provide pprint)

#| util functions |#
(define (half l p)
  (define mid (/ (length l) 2))
  (cond  ((equal? 'f p) (take l mid)) ;front
         ((equal? 'r p) (drop l mid)))) ;rear

(define (sf-list-ref lst pos)
  (if (or (< pos 0) (<= (length lst) pos))
    'NIL
    (list-ref lst pos)))


(define (loc-to-coord loc) ;location -> (i,j)
  (define (helper loc row col)
    (if (null? loc)
      (list (car row) (car col)) ;(= (car row) (cdr row))
      (let ()
        (define offset (expt 2 (sub1 (length loc))))
        (define floor-up-row (cons (+ offset (car row)) (cdr row)))
        (define floor-up-col (cons (+ offset (car col)) (cdr col)))
        (define ceiling-down-row (cons (car row) (- (cdr row) offset)))
        (define ceiling-down-col (cons (car col) (- (cdr col) offset)))
        (cond  ((= (car loc) 0) (helper (cdr loc) ceiling-down-row ceiling-down-col))
               ((= (car loc) 1) (helper (cdr loc) ceiling-down-row floor-up-col))
               ((= (car loc) 2) (helper (cdr loc) floor-up-row floor-up-col))
               ((= (car loc) 3) (helper (cdr loc) floor-up-row ceiling-down-col))))))
  (define size (expt 2 (length loc)))
  (helper loc (cons 1 size) (cons 1 size))); row & col: 1 ~ size 

(define (list-to-str l)
  (define (sym-to-str l)
    (cond  ((equal? black l) "B")
           ((equal? white l) "W")))
  (string-append (foldr string-append "" (map sym-to-str l)) "\n"))
#| util functions |#
;;; primitive tile

(define black ; black: form
  'B)
(define white ; white: form
  'W)


(define (glue-array-from-tree nw ne se sw) ;form * form * form * form -> form
  (tree-to-array (glue-tree-from-tree nw ne se sw)))

;DONE
(define (glue-array-from-array nw ne se sw) ;form * form * form * form -> form
  (define (sum w e)
    (if (null? w)
      '()
      (cons (append (car w) (car e)) (sum (cdr w) (cdr e)))))
  (if (list? nw)
    (cons 'array (append (sum (cdr nw) (cdr ne)) 
                         (sum (cdr sw) (cdr se))))
    (cons 'array (list (list nw ne) (list sw se)))))

(define (glue-tree-from-tree nw ne se sw) ; form * form * form * form -> form
  (if (list? nw)
    (cons 'tree (list (cdr nw) (cdr ne) (cdr se) (cdr sw)))
    (cons 'tree (list nw ne se sw))))

(define (glue-tree-from-array nw ne se sw) ; form * form * form * form -> form
  (if (list? nw)
    (cons 'tree (list 
                  (cdr (array-to-tree nw))
                  (cdr (array-to-tree ne))
                  (cdr (array-to-tree se))
                  (cdr (array-to-tree sw))))
    (cons 'tree (list nw ne se sw))
    ))

(define (rotate-array f) ; rotate-array: form -> form
  (define f-tree (array-to-tree f))
  (tree-to-array (rotate-tree f-tree)))

(define (neighbor-array location f) ; neighbor-array: location * form -> int
  (if (list? f)
    (let ()
      (define coord (loc-to-coord location)); int list
      (define row (car (loc-to-coord location))); int
      (define col (cadr (loc-to-coord location))); int
      (define f-array (cdr f))
      (define size (length f-array))
      (define row-up 
        (if (= row 1)
          '()
          (list-ref f-array (- row 2)))); list-ref start from 0
      (define row-cur (list-ref f-array (- row 1)))
      (define row-down
        (if (= row size)
          '()
          (list-ref f-array row)))
      (define res 
        (list ;just count the 8 tiles heh heh
          (sf-list-ref row-up (- col 2))
          (sf-list-ref row-up (- col 1))
          (sf-list-ref row-up col)
          (sf-list-ref row-cur (- col 2))
          (sf-list-ref row-cur col)
          (sf-list-ref row-down (- col 2))
          (sf-list-ref row-down (- col 1))
          (sf-list-ref row-down col)))
      (count (lambda (x) (equal? x black)) res))
    0))


(define (pprint-array f) ; pprint-array: form -> string
  (define (helper f)
    (if (null? f)
      ""
      (string-append (list-to-str (car f)) (helper (cdr f)))))

  (cond [(equal? black f) "B"]
        [(equal? white f) "W"]
        [(helper (cdr f))]))

(define (is-array? f) ; is-array?: form -> bool
  (cond [(equal? black f) #t]
        [(equal? white f) #t]
        [(equal? 'array (car f)) #t]
        [else #f]))


;;; implementation with tree

(define (rotate-tree f) ; rotate-tree: form -> form
  (define (helper f)
    (if (not (list? (car f)))
      (append (drop f 3) (take f 3))
      (list 
        (helper (list-ref f 3))
        (helper (list-ref f 0))
        (helper (list-ref f 1))
        (helper (list-ref f 2)))))

  (if (list? f)
    (cons 'tree (helper (cdr f)))
    f))


;narrow down coord by deviding by two

(define (neighbor-tree loc f) ; neighbor-tree: location * form -> int
  (neighbor-array loc (tree-to-array f)))

(define (pprint-tree f) ; pprint-tree: form -> string
  (pprint-array (tree-to-array f)))

(define (is-tree? f) ; is-tree?: form -> bool
  (cond [(equal? black f) #t]
        [(equal? white f) #t]
        [(equal? 'tree (car f)) #t]
        [else #f]))


;;; conversions 

(define (array-to-tree f) ; array-to-tree: form -> form
  (define (to-tree f) ;to-tree: (array) form * (tree) form -> (tree) form
    (if (not (list? f)) 
      f ;base case (white,black)
      (if (= (length f) 2) 
        (append (car f) (reverse (car (cdr f))))
        (let ()
          (list (to-tree (map (lambda (x) (half x 'f)) (half f 'f))) ;NW
                (to-tree (map (lambda (x) (half x 'r)) (half f 'f))) ;NE
                (to-tree (map (lambda (x) (half x 'r)) (half f 'r))) ;SE
                (to-tree (map (lambda (x) (half x 'f)) (half f 'r))) ;SW
                )))))
  (if (list? f)
    (cons 'tree (to-tree (cdr f)))
    f))


;TODO
(define (tree-to-array f) ; tree-to-array: form -> form
  (define (to-array f)
    (if (not (list? f))
      f;base case
      (if (= (length f) 1)  ;default 2*2 tree
        (append (half (car (half f 'f)) 'f) (half (car (half f 'r)) 'f))
        (let ()
          (glue-array-from-array (to-array (list-ref f 0)) ;NW
                                 (to-array (list-ref f 1)) ;NE
                                 (to-array (list-ref f 2)) ;SE
                                 (to-array (list-ref f 3))) ;SW
          ))))
  (if (list? f)
    (to-array (cdr f))
    f))

;;; interfaces

;always return tree implmentation
(define (glue nw ne se sw) ; glue: form * form * form * form -> form
  (define (new t)
    (if (is-tree? t)
      t
      (array-to-tree t)))
  (glue-tree-from-tree (new nw) (new ne) (new se) (new sw)))

(define (rotate f) ; rotate: form -> form
  (if (list? f)
    (if (is-array? f)
      (rotate-array f)
      (rotate-tree f))
    f)) ;1*1 tile


(define (neighbor loc f) ; neighbor: location * form -> int
  (if (is-array? f)
    (neighbor-array loc f)
    (neighbor-tree loc f)))

; In the document, it is said to have type form -> void, but implement
; as form -> string.
(define (pprint f) ; pprint: form -> string
  (if (is-array? f)
    (pprint-array f)
    (pprint-tree f)))

#| corner case tests |# 
(glue-array-from-tree 'B 'B 'W 'W)
(glue-array-from-array 'B 'B 'W 'W)
(glue-tree-from-array 'B 'B 'W 'W)
(glue-tree-from-tree 'B 'B 'W 'W)
(rotate-array 'B) ;B
(rotate-tree 'W) ;W
(neighbor-array '() 'B) ;0
(neighbor-tree '() 'W) ;0
(pprint-array 'B)
(pprint-tree 'W)
(array-to-tree 'B)
(tree-to-array 'W)
