#lang racket

(require "hw3-2-library.rkt")

(provide maze-check)
(provide get-next)

(define (maze-check maze start end) ;define & call a helper function
  ;maze * roomlist * roomset * room -> boolean
  (define (helper maze next-rooms curr-visited end)
    (if (is-member? end curr-visited)
      #t ;reached end
      (if (null? next-rooms)
        #f ;can't go anywhere
        ;can go on 
        (let () ;wrap defines with let
          ;because we are doing bfs, every room we can visit next time, will be visted
          ;so, by not visiting current rooms next time, we never visit same room again
          (define new-next (diff-set (get-next maze next-rooms) curr-visited))
          (define new-visited (add-set empty-set next-rooms))
          (helper maze new-next new-visited end)))))

  (helper maze (list start) empty-set end))

;next rooms you can visit
(define (get-next maze rooms) ;Maze * room list -> room list
  (remove-duplicates (if (null? rooms)
                       '()
                       (append (can-enter (car rooms) maze) (get-next maze (cdr rooms))))))

;Next-rooms - Visited rooms
(define (diff-set next-rooms visited-rooms) ;list * roomset -> list
  (filter (lambda (x) (not (is-member? x visited-rooms))) next-rooms))

(define (newdiff-set next-rooms visited-rooms) ;list * list -> list
  (filter (lambda (x) (not (has? x visited-rooms))) next-rooms))

;add to set, the elements in a list
(define (add-set roomset roomlist) ;list * set -> set
  (foldl add-element roomset roomlist))

;Elem * list -> bool
(define (has? E l) 
  (cond ((null? l) #f) 
        ((equal? E (car l)) #t) 
        (else (has? E (cdr l))))) 
