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

(define (half l p)
  (define mid (/ (length l) 2))
  (cond  ((equal? 'f p) (take l mid)) ;front
         ((equal? 'r p) (drop l mid)))) ;rear

(define test '(array (B B B B B B B B)(W B B B B B B B)(B B B B B B B B)(B B B B B B B B)(B B B B B B B B)(B B B B B B B B)(B B B B B B B B)(B B B B B B B W)))
(define test2 '(array (B W B W B W B W) (B W B W B W B W) (B W B W B W B W) (B W B W B W B W)(B W B W B W B W)(B W B W B W B W)(B W B W B W B W)(B W B W B W B W)))
;;; primitive tile

(define black ; black: form
  'B)
(define white ; white: form
  'W)

(define B black)
(define W white)

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

(define basic-array (glue-array-from-array B B B W))

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
  'TODO)

(define (list-to-str l)
  (define (sym-to-str l)
    (cond  ((equal? B l) "B")
           ((equal? W l) "W")))
  (string-append (foldr string-append "" (map sym-to-str l)) "\n"))

(define (pprint-array f) ; pprint-array: form -> string
  (define (helper f)
    (if (null? f)
      ""
      (string-append (list-to-str (car f)) (helper (cdr f)))))
  (helper (cdr f)))

(define (is-array? f) ; is-array?: form -> bool
  (cond [(equal? 'B f) #t]
        [(equal? 'W f) #t]
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

  (cons 'tree (helper (cdr f))))

(define (neighbor-tree loc f) ; neighbor-tree: location * form -> int
  'TODO)

(define (pprint-tree f) ; pprint-tree: form -> string
  (pprint-array (tree-to-array f)))

(define (is-tree? f) ; is-tree?: form -> bool
  (cond [(equal? 'B f) #t]
        [(equal? 'W f) #t]
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

  (to-array (cdr f)))
;(cdr testtree)
;(list-ref (cdr testtree) 0)
;(list-ref (list-ref (cdr testtree) 0) 0)

;;; interfaces

(define (glue nw ne se sw) ; glue: form * form * form * form -> form
  'TODO)

(define (rotate f) ; rotate: form -> form
  (if (is-array? f)
    (rotate-array f)
    (rotate-tree f)))

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
