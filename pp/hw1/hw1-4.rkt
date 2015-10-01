#lang racket
(provide leaf node)
(provide is-empty-tree? is-leaf? leaf-val nth-child)
(define (leaf n)
  (cons 'leaf n))

(define (node l)
  (cons 'node l))

(define (is-leaf? t)
  (if (eq? (car t) 'leaf) #t #f))

(define (leaf-val t)
  (if (is-leaf? t) (cdr t) #f))

(define (get-nth tl n)
  (if (null? tl) #f
      (if (= n 0) (car tl)
          (get-nth (cdr tl) (- n 1)))))

(define (nth-child t n)
  (if (eq? (car t) 'node)
            (get-nth (cdr t) n)
            #f))

;My code.
;(define (leaf n);leaf-tree
  ;(cons 'leaf n))

;(define (node l);list -> tree
  ;(cons 'tree l))

(define (is-empty-tree? t)
  (null? (cdr t)))

;(define (is-leaf? t)
  ;(equal? 'leaf (car t)))

;(define (is-tree? t)
  ;(equal? 'tree (car t)))

;(define (leaf-val t)
  ;(cdr t))

;(define (nth-child t n)
  ;(if (= n 0)
    ;(if (is-leaf? t)
      ;t
      ;(car (cdr t)))
      ;(nth-child (cdr t) (- n 1))))
