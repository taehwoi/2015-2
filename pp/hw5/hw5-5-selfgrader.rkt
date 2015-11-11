#lang racket

(require racket/match)
(require "common-grade.rkt")
(require "hw5-4.rkt")
(require "hw5-5.rkt")

(define B black)
(define W white)
(define basic (glue B B B W))
(define (turn pattern i)
  (if (<= i 0) 
      pattern
      (turn (rotate pattern) (- i 1))))
(define compound1
  (glue basic (turn basic 1) (turn basic 2) (turn basic 3)))
(define compound2
  (rotate (glue basic basic (rotate basic) (rotate basic))))
(define compound3
  (glue compound1 compound2 (turn compound1 2) (turn compound2 2)))

(define foo (rotate (array-to-tree (tree-to-array compound3))))
(define bar (rotate compound3))
;(equal (rotate compound3) (rotate (array-to-tree (tree-to-array compound3))))
;(equal (glue foo bar foo bar) (glue bar bar foo foo))
;(size (glue foo bar foo bar))
;(write-string (pprint basic))
;(size basic)

(define (all-coord f) ;form -> (coord) list
  (define width (size f))
  (permutations width '(0 1 2 3)))

(define (permutations size elements)
  (if (zero? size)
      '(())
      (append-map (lambda (p)
                    (map (lambda (e)
                           (cons e p))
                         elements))
                  (permutations (sub1 size) elements))))

(define compound4 (glue (rotate compound3) (rotate compound3) compound3 compound3))
(write-string (pprint compound4))
;(all-coord compound2)

;;; beautiful test

(sgoutput (lambda () (equal? 0 (size B))))
(sgoutput (lambda () (equal? 0 (size W))))
(sgoutput (lambda () (equal? 1 (size basic))))
(sgoutput (lambda () (equal? 2 (size compound1))))
(sgoutput (lambda () (equal? 2 (size compound2))))
(sgoutput (lambda () (equal? 3 (size compound3))))
(sgoutput (lambda () (equal? #t (beautiful compound1))))
(sgoutput (lambda () (equal? #f (beautiful compound2))))
(sgoutput (lambda () (equal? #t (beautiful compound3))))
(sgoutput (lambda () (equal? #f (beautiful compound4))))
