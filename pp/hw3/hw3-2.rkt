#lang racket

(require "hw3-2-library.rkt")

(provide maze-check)

(define (maze-check maze start end)
  (if (same-room? start end)
    #t
  (maze-check-helper maze (can-enter maze start) empty-set end)))

;maze * list * roomset * room -> boolean
(define (maze-check-helper maze next-rooms visited-rooms end)
  (if (null? next-rooms) 
    #f ;can't go anywhere
    (if (is-member? end next-rooms)
      #t ;can go there
      (let () ;wrap defines with let
       (define new-visited (add-set visited-rooms next-rooms))
       (define new-next (diff-set (get-next maze next-rooms) visited-rooms))
       (maze-check-helper maze new-next new-visited end)
       ))))

;Next-rooms - Visited rooms
(define (diff-set next-rooms visited-rooms) ;list * roomset -> list
  (filter (lambda (x) (not (is-member? x visited-rooms))) next-rooms))

;next rooms you can visit
(define (get-next maze rooms) ;Maze * room list -> room list
  (if (null? rooms )
    '()
    (append (can-enter maze (car rooms)) (get-next maze (cdr rooms)))))

;add to set, the elements in a list
(define (add-set roomset roomlist) ;list * set -> set
  (foldl add-element roomset roomlist))
