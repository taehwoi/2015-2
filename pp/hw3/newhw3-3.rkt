#lang racket

(require "hw3-3-library.rkt")

(provide mazeGen)

; Play around the interface of hw3-3-library.rkt.
; After executing this file, see hw3-3.ps.
; To read .ps files, you will need
;  - GhostScript: http://www.ghostscript.com/download/gsdnld.html
;  - Ghostview: http://pages.cs.wisc.edu/~ghost/gsview/ ; some test code: ;(define maze1 (init-maze 3 5)) ;(define maze2 (open-s 2 1 maze1))
;(maze-pp maze2)
(define dirs '(n ne se s sw nw))

(define (room i j)
  (list i j))
(define (room-x r)
  (car r))
(define (room-y y)
  (cadr y))
(define (random-element L)
  (list-ref L (random (length L))))
(define (diff-set next-rooms visited-rooms) ;list * roomset -> list
  (filter (lambda (x) (not (is-member? x visited-rooms))) next-rooms))

(define (all-rooms x y)
  (define (helper x y)
    (if (or (= x 0) (= y 0))
      '((0 0))
      (let ()
      (define half1 (cons (list (sub1 x) y) (helper (sub1 x) y)))
      (define half2 (cons (list x (sub1 y)) (helper x (sub1 y))))
      (append half2 half1)
      )))
  (remove-duplicates (cons (list (sub1 x) (sub1 y)) (helper (sub1 x) (sub1 y)))))

(define (det-next-i i j n)
  (if (even? j)
      (if (= n 0) i (+ i 1))
      (if (= n 0) (- i 1) i)
      ))

(define (get-room cur d)
  (let ([i (car cur)] [j (cadr cur)])
    (cond
      [(equal? d 'n) (append (list (- i 1) j))]
      [(equal? d 'ne) (append (list (det-next-i i j 0) (+ j 1)))]
      [(equal? d 'se) (append (list (det-next-i i j 1) (+ j 1)))]
      [(equal? d 's) (append (list (+ i 1) j))]
      [(equal? d 'sw) (append (list (det-next-i i j 1) (- j 1)))]
      [else (append (list (det-next-i i j 0) (- j 1)))]
      )
    ))

(define (rand-open d)
  (cond  [(equal? d 'n) open-n]
         [(equal? d 'ne) open-ne]
         [(equal? d 'se) open-se]
         [(equal? d 's) open-s]
         [(equal? d 'sw) open-sw]
         [(equal? d 'nw) open-nw]))

(define (addroom room l)
  (append (list room) l))

;(i * j) * roomlist -> bool
(define (is-member? i l)
  (if (null? l)
    #f
    (if (equal? i (car l))
      #t
      (is-member? i (cdr l)))))

(define (outofmaze? r n m)
  (or (> (room-x r) (- n 1)) (< (room-x r) 0) (> (room-y r) (- m 1)) (< (room-y r) 0)))

(define (mazeGen n m)
  (let ((maze (init-maze n m)))
    (define start (room 0 0))
    (define _maze (open-s (- n 1) (- m 1) (open-n 0 0 maze)))
    (maze-helper start _maze (addroom start '()) n m (all-rooms n m))))

;maze * stack -> maze
(define (maze-helper cur maze visited n m ar)
  (if (and (= (room-x cur) (- n 1)) (= (room-y cur) (- m 1))) ;reached end
    (if (null? ar) ;if made with enough complexity
      maze 
      (let ()
        ;(write ar)
        ;(newline)
        ;(write visited)
        ;(newline)
        (define randroom (random-element ar))
        ;(define unvisited (diff-set (all-rooms n m) visited))
        (maze-helper randroom maze (addroom randroom visited) n m (remove randroom ar))));make more
    (let ()
      (define dir (random-element dirs))
      (define nextroom (get-room cur dir))
      (define i (room-x cur))
      (define j (room-y cur))
      ;(define unvisited (diff-set (all-rooms n m) visited))
      (if (or (outofmaze? nextroom n m) (is-member? nextroom visited))
        (let ()
          (define randroom (random-element visited))
        (maze-helper randroom maze visited n m ar))
        (let ()
          (define nv (addroom nextroom visited))
          (maze-helper nextroom ((rand-open dir) i j maze) nv n m (remove nextroom ar)))))))

(define mz1 (mazeGen 9 9))
(maze-pp mz1)
