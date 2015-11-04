#lang racket

(require racket/match)
(require "common-grade.rkt")
(require "hw4-1.rkt")

(define (lookup key m)
  (match
   m
   ['() (raise "lookup failed")]
   [(cons (cons k v) rest)
    (if (equal? key k)
        v
        (lookup key rest))]))

(define (sum-list l)
  (define (sum-list-acc l acc)
    (match
     l
     ['() acc]
     [(cons x r) (sum-list-acc r (+ x acc))]))
  (sum-list-acc l 0))

(define (prefix? lhs rhs)
  (match
   lhs
   ['() #t]
   [(cons lhd ltl)
    (match
     rhs
     ['() #f]
     [(cons rhd rtl)
      (and (equal? lhd rhd) (prefix? ltl rtl))])]))

(define (forall? pred l)
  (match
   l
   ['() #t]
   [(cons hd tl) (and (pred hd) (forall? pred tl))]))

(define (prefix-list? str l)
  (forall? (lambda (x) (not (or (prefix? str x) (prefix? x str)))) l))

(define (prefixfree? l)
  (match
   l
   ['() #t]
   [(cons hd tl)
    (and (prefix-list? hd tl) (prefixfree? tl))]))

(define (wellformed? frequencies codes)
  (and
   (forall?
    (lambda (kf)
      (with-handlers
       ([(lambda (exn) #t) #f])
       (or
        (equal? (cdr kf) 0)
        (begin
          (lookup (car kf) codes)
          #t))))
    frequencies)
   (prefixfree? (map cdr codes))))

(define (compute-score frequencies codes)
  (sum-list
   (map
    (lambda (kc)
      (let* ([k (car kc)]
             [c (cdr kc)]
             [f (lookup k frequencies)])
        (* f (length c))))
    codes)))

(sgoutput 
 (lambda () 
   (let* ([frequencies (list (cons "a" 4) (cons "e" 4) (cons "f" 3) (cons "h" 2) (cons "i" 2) 
                             (cons "l" 1) (cons "m" 2) (cons "n" 2) (cons "o" 1) (cons "p" 1) 
                             (cons "r" 1) (cons "s" 2) (cons " " 7) (cons "t" 2) (cons "u" 1) 
                             (cons "x" 1))] 
          [codes (vlencode frequencies)] 
          [wf? (wellformed? frequencies codes)] 
          [score (compute-score frequencies codes)]) 
     (and wf? (equal? 135 score))))) 

(sgoutput 
 (lambda () 
   (let* ([frequencies (list (cons "a" 9) (cons "b" 3) (cons "c" 1) (cons "d" 1) (cons "e" 1) 
                             (cons "f" 1) (cons "g" 1) (cons "h" 1))] 
          [codes (vlencode frequencies)] 
          [wf? (wellformed? frequencies codes)] 
          [score (compute-score frequencies codes)]) 
     (and wf? (equal? 42 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "C" 67) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 67 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "V" 56) (cons "K" 44) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 100 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "b" 50) (cons "m" 82) (cons "H" 47) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 276 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "I" 13) (cons "Z" 89) (cons "f" 7) (cons "y" 7) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 157 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "N" 23) (cons "m" 98) (cons "E" 29) (cons "I" 87) (cons "g" 83) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 692 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "s" 74) (cons "I" 49) (cons "K" 35) (cons "n" 59) (cons "y" 58) (cons "C" 45) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 827 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "g" 37) (cons "b" 43) (cons "i" 62) (cons "d" 96) (cons "w" 29) (cons "G" 89) (cons "v" 29) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 1028 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "w" 92) (cons "s" 37) (cons "l" 97) (cons "n" 55) (cons "D" 53) (cons "R" 56) (cons "S" 89) (cons "Y" 29) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 1493 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "h" 33) (cons "s" 96) (cons "f" 4) (cons "k" 74) (cons "W" 70) (cons "i" 24) (cons "n" 3) (cons "E" 88) (cons "N" 19) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 1132 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "D" 28) (cons "t" 1) (cons "o" 29) (cons "S" 2) (cons "F" 79) (cons "K" 30) (cons "w" 43) (cons "r" 58) (cons "q" 45) (cons "T" 25) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 1028 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "n" 47) (cons "S" 79) (cons "k" 13) (cons "e" 62) (cons "R" 97) (cons "r" 67) (cons "p" 7) (cons "x" 78) (cons "E" 61) (cons "P" 13) (cons "d" 35) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 1798 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "K" 35) (cons "N" 59) (cons "G" 27) (cons "X" 49) (cons "L" 79) (cons "f" 3) (cons "T" 92) (cons "B" 55) (cons "m" 68) (cons "o" 57) (cons "S" 4) (cons "F" 7) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 1743 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "z" 60) (cons "X" 55) (cons "S" 54) (cons "Y" 3) (cons "C" 54) (cons "k" 93) (cons "R" 43) (cons "l" 13) (cons "b" 73) (cons "q" 35) (cons "c" 7) (cons "p" 62) (cons "v" 1) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 1854 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "R" 56) (cons "s" 36) (cons "z" 57) (cons "q" 27) (cons "X" 25) (cons "B" 64) (cons "T" 26) (cons "j" 29) (cons "S" 99) (cons "d" 49) (cons "U" 34) (cons "M" 18) (cons "y" 91) (cons "o" 71) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 2499 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "f" 68) (cons "h" 62) (cons "Y" 85) (cons "r" 60) (cons "N" 54) (cons "W" 82) (cons "j" 78) (cons "P" 91) (cons "s" 18) (cons "X" 21) (cons "t" 63) (cons "d" 60) (cons "F" 74) (cons "Z" 61) (cons "R" 41) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 3533 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "I" 36) (cons "v" 40) (cons "s" 91) (cons "r" 0) (cons "t" 14) (cons "E" 94) (cons "a" 85) (cons "c" 20) (cons "Q" 73) (cons "u" 37) (cons "N" 4) (cons "j" 0) (cons "x" 37) (cons "F" 80) (cons "L" 5) (cons "O" 46) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 2300 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "L" 9) (cons "W" 95) (cons "h" 89) (cons "D" 16) (cons "y" 26) (cons "x" 28) (cons "U" 6) (cons "u" 93) (cons "J" 68) (cons "w" 6) (cons "i" 46) (cons "K" 4) (cons "p" 10) (cons "M" 33) (cons "e" 95) (cons "F" 35) (cons "s" 9) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 2387 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "D" 23) (cons "q" 28) (cons "e" 50) (cons "v" 2) (cons "O" 41) (cons "c" 72) (cons "G" 95) (cons "R" 79) (cons "s" 93) (cons "z" 28) (cons "y" 63) (cons "S" 58) (cons "j" 92) (cons "k" 62) (cons "n" 21) (cons "A" 61) (cons "i" 14) (cons "g" 34) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 3628 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "e" 67) (cons "k" 30) (cons "D" 45) (cons "r" 85) (cons "a" 35) (cons "B" 83) (cons "y" 45) (cons "H" 20) (cons "g" 71) (cons "h" 53) (cons "S" 40) (cons "L" 18) (cons "W" 14) (cons "T" 30) (cons "l" 38) (cons "G" 99) (cons "t" 12) (cons "X" 14) (cons "N" 35) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 3373 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "J" 11) (cons "O" 51) (cons "r" 88) (cons "g" 9) (cons "L" 49) (cons "y" 9) (cons "K" 71) (cons "h" 26) (cons "G" 40) (cons "c" 17) (cons "R" 20) (cons "p" 78) (cons "A" 55) (cons "Z" 22) (cons "W" 6) (cons "e" 81) (cons "z" 99) (cons "k" 70) (cons "M" 88) (cons "X" 76) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 3904 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "z" 81) (cons "D" 84) (cons "E" 81) (cons "j" 74) (cons "i" 84) (cons "G" 44) (cons "h" 78) (cons "t" 76) (cons "C" 82) (cons "U" 5) (cons "O" 87) (cons "Y" 56) (cons "g" 80) (cons "I" 28) (cons "b" 86) (cons "B" 50) (cons "u" 36) (cons "S" 94) (cons "m" 66) (cons "r" 66) (cons "P" 10) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 5745 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "c" 21) (cons "J" 27) (cons "p" 12) (cons "d" 5) (cons "y" 53) (cons "o" 60) (cons "N" 17) (cons "B" 64) (cons "f" 67) (cons "Y" 29) (cons "h" 70) (cons "i" 48) (cons "m" 36) (cons "H" 78) (cons "r" 46) (cons "v" 90) (cons "U" 1) (cons "L" 96) (cons "V" 82) (cons "R" 85) (cons "t" 40) (cons "b" 11) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 4345 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "P" 69) (cons "m" 86) (cons "n" 60) (cons "K" 23) (cons "N" 17) (cons "G" 14) (cons "V" 44) (cons "x" 60) (cons "v" 93) (cons "U" 54) (cons "B" 28) (cons "S" 64) (cons "y" 10) (cons "l" 55) (cons "C" 64) (cons "M" 76) (cons "a" 79) (cons "p" 2) (cons "t" 6) (cons "T" 10) (cons "q" 6) (cons "X" 93) (cons "o" 96) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 4658 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "u" 9) (cons "x" 47) (cons "y" 99) (cons "G" 59) (cons "P" 6) (cons "N" 98) (cons "Z" 14) (cons "T" 80) (cons "l" 48) (cons "W" 85) (cons "D" 77) (cons "m" 15) (cons "I" 25) (cons "r" 47) (cons "n" 86) (cons "Y" 88) (cons "M" 78) (cons "Q" 8) (cons "t" 68) (cons "K" 65) (cons "k" 83) (cons "B" 99) (cons "s" 80) (cons "R" 62) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 6223 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "D" 8) (cons "R" 36) (cons "S" 91) (cons "t" 41) (cons "d" 79) (cons "e" 8) (cons "o" 27) (cons "f" 91) (cons "T" 8) (cons "W" 43) (cons "r" 96) (cons "x" 7) (cons "F" 95) (cons "P" 26) (cons "Z" 67) (cons "n" 12) (cons "z" 52) (cons "H" 93) (cons "l" 38) (cons "E" 32) (cons "A" 91) (cons "y" 15) (cons "G" 97) (cons "j" 55) (cons "s" 77) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 5582 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "M" 78) (cons "m" 90) (cons "E" 21) (cons "B" 78) (cons "q" 66) (cons "Y" 4) (cons "i" 12) (cons "p" 23) (cons "I" 11) (cons "t" 32) (cons "F" 21) (cons "f" 13) (cons "b" 10) (cons "s" 41) (cons "Z" 3) (cons "l" 77) (cons "y" 42) (cons "d" 32) (cons "U" 13) (cons "c" 65) (cons "g" 29) (cons "Q" 17) (cons "R" 79) (cons "S" 46) (cons "o" 96) (cons "a" 18) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 4414 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "u" 6) (cons "H" 79) (cons "D" 47) (cons "k" 9) (cons "n" 5) (cons "J" 54) (cons "R" 76) (cons "Z" 7) (cons "E" 83) (cons "W" 20) (cons "A" 62) (cons "G" 57) (cons "t" 27) (cons "z" 50) (cons "N" 4) (cons "v" 16) (cons "F" 14) (cons "C" 38) (cons "c" 32) (cons "B" 55) (cons "a" 73) (cons "i" 46) (cons "V" 70) (cons "P" 29) (cons "w" 40) (cons "x" 3) (cons "o" 19) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 4508 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "W" 52) (cons "c" 99) (cons "t" 77) (cons "I" 43) (cons "r" 12) (cons "i" 28) (cons "l" 52) (cons "e" 77) (cons "R" 95) (cons "O" 65) (cons "C" 43) (cons "z" 23) (cons "u" 71) (cons "F" 17) (cons "J" 24) (cons "p" 59) (cons "k" 13) (cons "a" 41) (cons "y" 65) (cons "P" 5) (cons "q" 36) (cons "U" 5) (cons "V" 79) (cons "x" 53) (cons "o" 51) (cons "H" 28) (cons "v" 69) (cons "T" 2) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 5836 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "O" 5) (cons "P" 84) (cons "E" 17) (cons "M" 91) (cons "J" 89) (cons "f" 43) (cons "t" 62) (cons "o" 35) (cons "v" 87) (cons "u" 17) (cons "T" 73) (cons "D" 10) (cons "S" 28) (cons "i" 77) (cons "d" 43) (cons "e" 32) (cons "V" 64) (cons "Y" 12) (cons "b" 77) (cons "k" 95) (cons "K" 7) (cons "l" 76) (cons "c" 13) (cons "G" 18) (cons "m" 67) (cons "z" 57) (cons "x" 33) (cons "w" 73) (cons "A" 11) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 6381 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "A" 29) (cons "I" 37) (cons "Q" 77) (cons "J" 5) (cons "R" 1) (cons "K" 51) (cons "c" 64) (cons "W" 4) (cons "n" 83) (cons "C" 17) (cons "H" 14) (cons "r" 15) (cons "O" 6) (cons "u" 57) (cons "l" 15) (cons "i" 50) (cons "Y" 78) (cons "m" 94) (cons "F" 93) (cons "e" 88) (cons "M" 29) (cons "a" 4) (cons "s" 78) (cons "v" 16) (cons "B" 22) (cons "g" 5) (cons "S" 21) (cons "L" 52) (cons "p" 3) (cons "x" 76) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 5251 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "U" 74) (cons "w" 9) (cons "x" 77) (cons "z" 89) (cons "t" 17) (cons "r" 68) (cons "n" 8) (cons "Z" 69) (cons "N" 79) (cons "l" 10) (cons "O" 92) (cons "A" 30) (cons "R" 37) (cons "q" 38) (cons "B" 77) (cons "S" 78) (cons "s" 49) (cons "F" 31) (cons "h" 19) (cons "X" 36) (cons "Y" 96) (cons "j" 27) (cons "d" 70) (cons "H" 24) (cons "T" 41) (cons "p" 70) (cons "K" 11) (cons "k" 33) (cons "E" 44) (cons "I" 23) (cons "e" 98) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 7210 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "U" 2) (cons "k" 7) (cons "h" 5) (cons "c" 75) (cons "L" 74) (cons "I" 82) (cons "w" 50) (cons "p" 84) (cons "x" 97) (cons "Q" 30) (cons "u" 12) (cons "Z" 11) (cons "R" 73) (cons "q" 96) (cons "v" 11) (cons "Y" 29) (cons "C" 39) (cons "J" 71) (cons "G" 22) (cons "S" 63) (cons "g" 17) (cons "b" 93) (cons "n" 68) (cons "m" 63) (cons "z" 60) (cons "t" 79) (cons "D" 76) (cons "i" 5) (cons "K" 37) (cons "V" 55) (cons "M" 70) (cons "H" 50) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 7588 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "Z" 37) (cons "e" 16) (cons "D" 15) (cons "v" 17) (cons "M" 68) (cons "V" 83) (cons "r" 0) (cons "h" 58) (cons "U" 74) (cons "C" 64) (cons "c" 37) (cons "m" 53) (cons "O" 52) (cons "Q" 6) (cons "k" 73) (cons "R" 27) (cons "o" 94) (cons "i" 65) (cons "W" 7) (cons "z" 82) (cons "F" 33) (cons "N" 73) (cons "y" 4) (cons "u" 51) (cons "g" 66) (cons "p" 75) (cons "T" 87) (cons "f" 57) (cons "K" 61) (cons "H" 99) (cons "P" 91) (cons "I" 16) (cons "s" 86) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 8282 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "t" 9) (cons "C" 72) (cons "u" 94) (cons "g" 45) (cons "R" 62) (cons "d" 74) (cons "a" 70) (cons "f" 26) (cons "c" 50) (cons "i" 78) (cons "H" 49) (cons "Z" 28) (cons "N" 27) (cons "T" 90) (cons "x" 20) (cons "V" 90) (cons "I" 10) (cons "F" 9) (cons "h" 24) (cons "m" 51) (cons "w" 15) (cons "E" 76) (cons "q" 44) (cons "o" 85) (cons "v" 98) (cons "y" 59) (cons "A" 72) (cons "k" 57) (cons "Y" 86) (cons "l" 14) (cons "P" 24) (cons "n" 11) (cons "Q" 12) (cons "G" 62) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 8212 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "v" 58) (cons "T" 37) (cons "g" 32) (cons "B" 57) (cons "G" 19) (cons "J" 37) (cons "U" 35) (cons "E" 76) (cons "P" 49) (cons "R" 84) (cons "k" 1) (cons "y" 88) (cons "u" 64) (cons "i" 58) (cons "c" 98) (cons "n" 19) (cons "m" 2) (cons "V" 26) (cons "s" 17) (cons "I" 66) (cons "o" 76) (cons "d" 52) (cons "A" 65) (cons "h" 93) (cons "r" 98) (cons "e" 21) (cons "H" 37) (cons "z" 6) (cons "Y" 60) (cons "O" 2) (cons "S" 2) (cons "D" 82) (cons "C" 87) (cons "M" 15) (cons "w" 10) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 7801 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "q" 78) (cons "P" 12) (cons "L" 85) (cons "A" 36) (cons "f" 79) (cons "V" 10) (cons "k" 14) (cons "X" 72) (cons "C" 20) (cons "E" 3) (cons "U" 22) (cons "a" 65) (cons "g" 25) (cons "x" 12) (cons "R" 0) (cons "N" 10) (cons "i" 88) (cons "z" 76) (cons "w" 75) (cons "e" 29) (cons "Q" 11) (cons "n" 37) (cons "S" 52) (cons "r" 14) (cons "y" 84) (cons "M" 14) (cons "G" 59) (cons "m" 28) (cons "b" 29) (cons "H" 2) (cons "s" 17) (cons "Y" 61) (cons "T" 42) (cons "h" 39) (cons "j" 41) (cons "B" 28) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 6565 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "E" 67) (cons "I" 8) (cons "m" 41) (cons "K" 78) (cons "P" 9) (cons "Z" 32) (cons "t" 32) (cons "g" 7) (cons "C" 57) (cons "H" 98) (cons "a" 53) (cons "r" 67) (cons "M" 98) (cons "X" 71) (cons "w" 17) (cons "s" 53) (cons "L" 36) (cons "G" 85) (cons "b" 17) (cons "B" 26) (cons "D" 13) (cons "v" 1) (cons "k" 25) (cons "p" 57) (cons "V" 80) (cons "o" 76) (cons "W" 5) (cons "e" 76) (cons "O" 33) (cons "F" 12) (cons "q" 34) (cons "f" 30) (cons "Q" 18) (cons "S" 69) (cons "U" 28) (cons "n" 55) (cons "u" 74) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 8071 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "M" 99) (cons "Z" 12) (cons "i" 49) (cons "a" 13) (cons "z" 89) (cons "G" 36) (cons "R" 24) (cons "I" 25) (cons "P" 80) (cons "E" 54) (cons "t" 19) (cons "O" 0) (cons "S" 62) (cons "r" 62) (cons "K" 21) (cons "F" 68) (cons "g" 30) (cons "w" 41) (cons "q" 95) (cons "v" 41) (cons "N" 90) (cons "j" 8) (cons "W" 11) (cons "k" 8) (cons "x" 20) (cons "B" 45) (cons "m" 38) (cons "T" 68) (cons "H" 54) (cons "l" 30) (cons "n" 98) (cons "L" 37) (cons "Y" 61) (cons "s" 37) (cons "d" 69) (cons "h" 83) (cons "X" 77) (cons "u" 13) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 8804 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "C" 1) (cons "u" 35) (cons "I" 43) (cons "W" 57) (cons "U" 83) (cons "e" 22) (cons "F" 18) (cons "H" 10) (cons "O" 43) (cons "d" 22) (cons "M" 70) (cons "m" 90) (cons "K" 58) (cons "h" 83) (cons "D" 67) (cons "z" 97) (cons "L" 63) (cons "X" 82) (cons "s" 74) (cons "N" 28) (cons "y" 30) (cons "E" 43) (cons "g" 36) (cons "p" 90) (cons "R" 28) (cons "r" 53) (cons "Z" 75) (cons "Y" 49) (cons "c" 10) (cons "q" 52) (cons "v" 88) (cons "G" 35) (cons "A" 54) (cons "T" 62) (cons "o" 66) (cons "S" 88) (cons "J" 99) (cons "k" 36) (cons "a" 65) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 10794 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "t" 6) (cons "y" 27) (cons "X" 19) (cons "A" 8) (cons "U" 41) (cons "k" 95) (cons "M" 65) (cons "e" 9) (cons "Y" 7) (cons "c" 78) (cons "i" 57) (cons "J" 48) (cons "S" 32) (cons "u" 60) (cons "z" 98) (cons "Q" 93) (cons "j" 52) (cons "m" 76) (cons "P" 51) (cons "d" 58) (cons "R" 20) (cons "h" 87) (cons "q" 32) (cons "K" 51) (cons "b" 45) (cons "s" 64) (cons "G" 6) (cons "l" 39) (cons "C" 90) (cons "f" 79) (cons "o" 22) (cons "E" 21) (cons "g" 37) (cons "v" 77) (cons "B" 62) (cons "r" 94) (cons "Z" 42) (cons "N" 35) (cons "x" 62) (cons "D" 83) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 10374 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "K" 87) (cons "x" 40) (cons "L" 8) (cons "J" 20) (cons "M" 66) (cons "c" 42) (cons "O" 42) (cons "H" 85) (cons "D" 99) (cons "a" 33) (cons "j" 67) (cons "h" 83) (cons "e" 25) (cons "N" 61) (cons "q" 37) (cons "w" 48) (cons "d" 64) (cons "b" 21) (cons "o" 93) (cons "R" 55) (cons "z" 33) (cons "y" 80) (cons "f" 3) (cons "B" 57) (cons "U" 18) (cons "v" 96) (cons "Y" 76) (cons "l" 90) (cons "I" 41) (cons "T" 60) (cons "S" 32) (cons "m" 34) (cons "F" 66) (cons "X" 18) (cons "r" 20) (cons "u" 40) (cons "W" 8) (cons "k" 13) (cons "n" 39) (cons "G" 21) (cons "C" 10) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 9914 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "j" 68) (cons "R" 28) (cons "F" 60) (cons "H" 2) (cons "c" 33) (cons "e" 91) (cons "w" 19) (cons "L" 73) (cons "u" 35) (cons "K" 60) (cons "b" 89) (cons "O" 19) (cons "J" 24) (cons "I" 5) (cons "x" 96) (cons "n" 79) (cons "y" 89) (cons "S" 82) (cons "l" 13) (cons "M" 21) (cons "N" 64) (cons "s" 0) (cons "q" 5) (cons "C" 18) (cons "T" 85) (cons "r" 20) (cons "a" 62) (cons "X" 17) (cons "B" 54) (cons "i" 34) (cons "d" 6) (cons "p" 10) (cons "v" 86) (cons "t" 94) (cons "G" 15) (cons "Y" 31) (cons "P" 60) (cons "U" 70) (cons "o" 16) (cons "E" 54) (cons "z" 17) (cons "A" 66) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 9431 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "B" 97) (cons "b" 94) (cons "S" 50) (cons "d" 80) (cons "a" 5) (cons "l" 37) (cons "w" 23) (cons "X" 31) (cons "M" 70) (cons "k" 85) (cons "D" 10) (cons "f" 43) (cons "r" 97) (cons "o" 5) (cons "j" 6) (cons "m" 50) (cons "A" 75) (cons "g" 46) (cons "q" 12) (cons "E" 86) (cons "s" 41) (cons "p" 69) (cons "c" 0) (cons "H" 30) (cons "G" 58) (cons "t" 80) (cons "C" 75) (cons "O" 53) (cons "L" 28) (cons "Z" 27) (cons "F" 60) (cons "N" 74) (cons "W" 70) (cons "T" 30) (cons "U" 42) (cons "u" 34) (cons "J" 29) (cons "i" 36) (cons "R" 28) (cons "Y" 13) (cons "K" 67) (cons "P" 52) (cons "x" 92) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 10840 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "U" 69) (cons "A" 64) (cons "H" 0) (cons "f" 19) (cons "O" 28) (cons "m" 45) (cons "F" 83) (cons "i" 77) (cons "C" 19) (cons "p" 70) (cons "z" 25) (cons "V" 34) (cons "y" 41) (cons "v" 75) (cons "c" 52) (cons "d" 79) (cons "P" 28) (cons "b" 20) (cons "W" 59) (cons "I" 75) (cons "D" 34) (cons "E" 61) (cons "S" 98) (cons "X" 37) (cons "g" 18) (cons "e" 60) (cons "Q" 14) (cons "T" 38) (cons "a" 50) (cons "h" 93) (cons "L" 28) (cons "Y" 93) (cons "N" 54) (cons "r" 25) (cons "u" 65) (cons "R" 76) (cons "l" 35) (cons "w" 60) (cons "Z" 88) (cons "x" 71) (cons "B" 37) (cons "k" 69) (cons "n" 42) (cons "j" 98) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12212 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "L" 84) (cons "s" 95) (cons "O" 17) (cons "R" 52) (cons "t" 73) (cons "H" 46) (cons "K" 90) (cons "W" 81) (cons "E" 14) (cons "Y" 40) (cons "u" 5) (cons "m" 12) (cons "p" 70) (cons "F" 38) (cons "G" 9) (cons "D" 92) (cons "a" 29) (cons "f" 30) (cons "Q" 50) (cons "x" 35) (cons "Z" 20) (cons "k" 7) (cons "w" 14) (cons "C" 22) (cons "U" 95) (cons "N" 56) (cons "l" 70) (cons "i" 18) (cons "T" 59) (cons "g" 36) (cons "V" 24) (cons "I" 11) (cons "b" 13) (cons "c" 82) (cons "d" 28) (cons "z" 34) (cons "o" 30) (cons "v" 28) (cons "P" 78) (cons "h" 55) (cons "q" 14) (cons "n" 46) (cons "r" 39) (cons "A" 67) (cons "e" 65) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 10336 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "K" 80) (cons "Y" 27) (cons "g" 86) (cons "r" 59) (cons "l" 69) (cons "x" 51) (cons "t" 67) (cons "h" 11) (cons "m" 25) (cons "k" 81) (cons "v" 50) (cons "B" 72) (cons "A" 0) (cons "I" 22) (cons "q" 61) (cons "L" 91) (cons "y" 7) (cons "i" 3) (cons "j" 23) (cons "V" 67) (cons "U" 85) (cons "a" 95) (cons "O" 13) (cons "Z" 91) (cons "G" 26) (cons "p" 41) (cons "D" 50) (cons "s" 9) (cons "C" 34) (cons "u" 93) (cons "X" 70) (cons "R" 26) (cons "Q" 67) (cons "w" 39) (cons "H" 30) (cons "T" 62) (cons "n" 67) (cons "z" 69) (cons "b" 96) (cons "S" 25) (cons "d" 99) (cons "M" 81) (cons "W" 40) (cons "e" 15) (cons "F" 83) (cons "E" 76) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12867 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "Q" 77) (cons "p" 96) (cons "m" 58) (cons "E" 98) (cons "c" 49) (cons "K" 76) (cons "v" 21) (cons "j" 49) (cons "A" 76) (cons "I" 11) (cons "x" 12) (cons "n" 80) (cons "X" 63) (cons "b" 69) (cons "d" 8) (cons "D" 25) (cons "y" 12) (cons "t" 55) (cons "L" 67) (cons "o" 67) (cons "M" 34) (cons "B" 1) (cons "r" 46) (cons "H" 71) (cons "u" 10) (cons "Y" 38) (cons "T" 52) (cons "C" 95) (cons "P" 39) (cons "f" 81) (cons "k" 30) (cons "g" 15) (cons "O" 39) (cons "G" 35) (cons "q" 34) (cons "U" 94) (cons "w" 66) (cons "S" 67) (cons "e" 50) (cons "F" 80) (cons "a" 4) (cons "V" 77) (cons "z" 44) (cons "N" 50) (cons "i" 75) (cons "J" 84) (cons "h" 84) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13158 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "w" 98) (cons "T" 92) (cons "j" 84) (cons "Z" 65) (cons "H" 64) (cons "u" 7) (cons "E" 72) (cons "s" 16) (cons "O" 19) (cons "Y" 89) (cons "C" 37) (cons "J" 75) (cons "P" 94) (cons "b" 60) (cons "X" 6) (cons "r" 70) (cons "o" 96) (cons "g" 69) (cons "n" 15) (cons "x" 16) (cons "K" 68) (cons "h" 21) (cons "G" 54) (cons "l" 4) (cons "d" 30) (cons "c" 47) (cons "i" 2) (cons "Q" 7) (cons "A" 50) (cons "q" 53) (cons "V" 41) (cons "z" 38) (cons "k" 72) (cons "D" 29) (cons "F" 38) (cons "S" 60) (cons "B" 81) (cons "U" 11) (cons "L" 59) (cons "e" 26) (cons "W" 95) (cons "m" 32) (cons "R" 38) (cons "M" 6) (cons "f" 63) (cons "y" 52) (cons "I" 21) (cons "N" 24) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12037 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "G" 48) (cons "C" 27) (cons "r" 57) (cons "J" 64) (cons "N" 26) (cons "y" 24) (cons "F" 78) (cons "Z" 93) (cons "p" 33) (cons "e" 78) (cons "X" 4) (cons "H" 55) (cons "c" 45) (cons "l" 6) (cons "I" 96) (cons "d" 84) (cons "x" 13) (cons "a" 29) (cons "u" 21) (cons "t" 30) (cons "g" 80) (cons "o" 33) (cons "D" 86) (cons "v" 61) (cons "Y" 81) (cons "h" 66) (cons "w" 23) (cons "R" 78) (cons "U" 70) (cons "W" 51) (cons "S" 2) (cons "b" 66) (cons "A" 67) (cons "i" 43) (cons "E" 34) (cons "O" 5) (cons "K" 12) (cons "j" 70) (cons "z" 59) (cons "f" 90) (cons "s" 3) (cons "k" 23) (cons "T" 16) (cons "M" 65) (cons "V" 55) (cons "B" 61) (cons "Q" 55) (cons "P" 57) (cons "L" 70) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12874 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "K" 36) (cons "A" 22) (cons "r" 24) (cons "B" 73) (cons "J" 25) (cons "I" 24) (cons "T" 4) (cons "U" 48) (cons "a" 17) (cons "X" 56) (cons "V" 28) (cons "s" 98) (cons "Q" 30) (cons "t" 43) (cons "o" 98) (cons "N" 77) (cons "S" 39) (cons "H" 72) (cons "q" 89) (cons "P" 74) (cons "v" 15) (cons "w" 84) (cons "f" 34) (cons "R" 74) (cons "O" 18) (cons "L" 23) (cons "E" 64) (cons "l" 22) (cons "C" 74) (cons "M" 64) (cons "i" 31) (cons "y" 90) (cons "D" 91) (cons "G" 53) (cons "b" 53) (cons "F" 73) (cons "u" 64) (cons "n" 26) (cons "Y" 55) (cons "W" 21) (cons "Z" 26) (cons "g" 64) (cons "c" 33) (cons "z" 61) (cons "m" 67) (cons "x" 21) (cons "d" 82) (cons "k" 15) (cons "j" 55) (cons "h" 51) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13564 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "S" 82) (cons "a" 87) (cons "U" 94) (cons "C" 98) (cons "t" 22) (cons "l" 31) (cons "Y" 3) (cons "f" 63) (cons "x" 31) (cons "b" 83) (cons "Z" 29) (cons "c" 87) (cons "J" 17) (cons "z" 37) (cons "O" 64) (cons "m" 47) (cons "G" 43) (cons "R" 62) (cons "h" 41) (cons "P" 48) (cons "v" 13) (cons "o" 53) (cons "Q" 98) (cons "g" 36) (cons "u" 64) (cons "W" 33) (cons "A" 33) (cons "n" 73) (cons "w" 92) (cons "N" 58) (cons "I" 86) (cons "j" 71) (cons "e" 97) (cons "F" 2) (cons "s" 5) (cons "B" 42) (cons "X" 71) (cons "T" 79) (cons "M" 3) (cons "E" 23) (cons "D" 94) (cons "i" 64) (cons "y" 29) (cons "k" 59) (cons "V" 5) (cons "q" 97) (cons "K" 97) (cons "L" 40) (cons "p" 16) (cons "H" 11) (cons "r" 97) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14665 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "K" 55) (cons "s" 96) (cons "O" 14) (cons "J" 35) (cons "C" 12) (cons "e" 38) (cons "y" 74) (cons "t" 63) (cons "q" 86) (cons "v" 40) (cons "g" 96) (cons "j" 48) (cons "z" 63) (cons "d" 53) (cons "G" 98) (cons "l" 91) (cons "T" 6) (cons "D" 90) (cons "M" 88) (cons "Y" 44) (cons "i" 67) (cons "S" 65) (cons "U" 96) (cons "H" 43) (cons "X" 77) (cons "c" 22) (cons "R" 27) (cons "w" 35) (cons "r" 13) (cons "F" 6) (cons "A" 3) (cons "L" 95) (cons "Q" 78) (cons "P" 19) (cons "W" 23) (cons "o" 79) (cons "E" 61) (cons "B" 76) (cons "x" 8) (cons "Z" 23) (cons "N" 78) (cons "a" 20) (cons "p" 50) (cons "f" 34) (cons "h" 89) (cons "I" 48) (cons "n" 14) (cons "V" 82) (cons "u" 51) (cons "b" 99) (cons "m" 52) (cons "k" 12) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14903 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "u" 48) (cons "G" 66) (cons "M" 30) (cons "q" 62) (cons "L" 96) (cons "c" 67) (cons "X" 85) (cons "t" 22) (cons "F" 55) (cons "H" 30) (cons "y" 69) (cons "K" 90) (cons "T" 83) (cons "V" 94) (cons "x" 83) (cons "W" 36) (cons "E" 12) (cons "z" 77) (cons "d" 77) (cons "I" 39) (cons "S" 97) (cons "g" 60) (cons "J" 30) (cons "m" 19) (cons "a" 3) (cons "C" 2) (cons "N" 45) (cons "o" 44) (cons "v" 40) (cons "Y" 16) (cons "O" 30) (cons "i" 28) (cons "R" 35) (cons "b" 72) (cons "P" 15) (cons "k" 4) (cons "w" 46) (cons "h" 9) (cons "D" 84) (cons "l" 2) (cons "n" 45) (cons "p" 36) (cons "Q" 7) (cons "A" 51) (cons "s" 93) (cons "B" 21) (cons "U" 49) (cons "r" 75) (cons "f" 4) (cons "j" 78) (cons "e" 44) (cons "Z" 29) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13176 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "k" 79) (cons "l" 14) (cons "e" 68) (cons "o" 80) (cons "L" 0) (cons "Q" 40) (cons "S" 93) (cons "A" 54) (cons "v" 17) (cons "G" 54) (cons "M" 22) (cons "s" 22) (cons "B" 24) (cons "O" 11) (cons "b" 77) (cons "C" 90) (cons "h" 90) (cons "R" 84) (cons "t" 82) (cons "D" 76) (cons "I" 23) (cons "f" 0) (cons "y" 84) (cons "H" 56) (cons "W" 8) (cons "Z" 63) (cons "q" 85) (cons "i" 49) (cons "g" 57) (cons "E" 86) (cons "d" 98) (cons "Y" 77) (cons "z" 43) (cons "u" 10) (cons "j" 70) (cons "w" 91) (cons "p" 16) (cons "J" 37) (cons "V" 24) (cons "r" 39) (cons "x" 38) (cons "a" 23) (cons "T" 96) (cons "F" 36) (cons "P" 36) (cons "K" 76) (cons "N" 70) (cons "c" 97) (cons "m" 96) (cons "U" 68) (cons "X" 11) (cons "n" 72) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 15318 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "I" 91) (cons "H" 93) (cons "K" 88) (cons "R" 93) (cons "g" 3) (cons "i" 94) (cons "v" 7) (cons "O" 13) (cons "t" 19) (cons "z" 19) (cons "A" 20) (cons "X" 12) (cons "e" 75) (cons "c" 23) (cons "S" 22) (cons "C" 9) (cons "T" 54) (cons "s" 33) (cons "D" 77) (cons "F" 71) (cons "o" 2) (cons "N" 32) (cons "L" 92) (cons "x" 90) (cons "h" 82) (cons "w" 70) (cons "G" 92) (cons "U" 60) (cons "P" 49) (cons "Q" 4) (cons "J" 97) (cons "V" 71) (cons "u" 57) (cons "d" 18) (cons "r" 88) (cons "W" 70) (cons "E" 4) (cons "m" 79) (cons "a" 35) (cons "p" 23) (cons "y" 98) (cons "M" 25) (cons "Y" 36) (cons "f" 0) (cons "Z" 57) (cons "n" 39) (cons "B" 32) (cons "b" 51) (cons "j" 48) (cons "l" 73) (cons "q" 73) (cons "k" 51) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14072 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "Y" 1) (cons "s" 7) (cons "b" 4) (cons "F" 18) (cons "w" 50) (cons "S" 68) (cons "f" 79) (cons "e" 48) (cons "r" 97) (cons "W" 1) (cons "M" 22) (cons "Q" 14) (cons "x" 28) (cons "C" 88) (cons "U" 3) (cons "o" 72) (cons "t" 0) (cons "d" 44) (cons "q" 43) (cons "A" 89) (cons "m" 72) (cons "v" 96) (cons "T" 50) (cons "H" 88) (cons "I" 80) (cons "n" 13) (cons "l" 58) (cons "G" 18) (cons "c" 63) (cons "K" 24) (cons "g" 69) (cons "R" 6) (cons "Z" 41) (cons "z" 60) (cons "P" 39) (cons "h" 9) (cons "N" 54) (cons "X" 38) (cons "u" 16) (cons "D" 61) (cons "y" 45) (cons "L" 7) (cons "O" 19) (cons "a" 55) (cons "B" 29) (cons "i" 84) (cons "J" 31) (cons "k" 82) (cons "V" 94) (cons "E" 80) (cons "j" 36) (cons "p" 81) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12723 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "k" 71) (cons "L" 44) (cons "q" 66) (cons "v" 66) (cons "b" 39) (cons "M" 48) (cons "B" 27) (cons "Y" 8) (cons "D" 58) (cons "J" 76) (cons "u" 25) (cons "t" 4) (cons "c" 96) (cons "z" 60) (cons "V" 14) (cons "O" 35) (cons "H" 72) (cons "p" 75) (cons "e" 64) (cons "K" 66) (cons "x" 70) (cons "w" 63) (cons "r" 13) (cons "W" 42) (cons "o" 93) (cons "g" 21) (cons "s" 38) (cons "Q" 60) (cons "I" 81) (cons "G" 31) (cons "a" 1) (cons "A" 60) (cons "h" 49) (cons "T" 45) (cons "n" 66) (cons "y" 24) (cons "R" 79) (cons "j" 32) (cons "P" 26) (cons "Z" 49) (cons "S" 81) (cons "X" 89) (cons "E" 35) (cons "F" 6) (cons "N" 98) (cons "l" 35) (cons "m" 7) (cons "d" 46) (cons "f" 79) (cons "U" 64) (cons "i" 17) (cons "C" 44) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14050 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "O" 0) (cons "p" 85) (cons "M" 35) (cons "e" 79) (cons "X" 99) (cons "r" 5) (cons "V" 9) (cons "Y" 89) (cons "J" 58) (cons "Q" 94) (cons "h" 93) (cons "N" 34) (cons "a" 68) (cons "G" 95) (cons "C" 98) (cons "t" 92) (cons "U" 42) (cons "w" 59) (cons "g" 20) (cons "x" 3) (cons "i" 41) (cons "S" 52) (cons "u" 73) (cons "P" 24) (cons "b" 10) (cons "L" 48) (cons "q" 48) (cons "o" 8) (cons "k" 31) (cons "D" 10) (cons "m" 68) (cons "y" 35) (cons "H" 48) (cons "d" 56) (cons "E" 3) (cons "I" 47) (cons "T" 18) (cons "A" 55) (cons "c" 22) (cons "Z" 31) (cons "B" 93) (cons "v" 1) (cons "R" 77) (cons "W" 7) (cons "F" 27) (cons "n" 72) (cons "j" 80) (cons "K" 5) (cons "l" 73) (cons "s" 8) (cons "z" 48) (cons "f" 39) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12926 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "N" 38) (cons "T" 96) (cons "a" 21) (cons "G" 40) (cons "L" 93) (cons "n" 72) (cons "W" 30) (cons "v" 11) (cons "V" 12) (cons "g" 84) (cons "s" 72) (cons "X" 10) (cons "h" 69) (cons "t" 69) (cons "i" 83) (cons "E" 78) (cons "j" 22) (cons "O" 23) (cons "B" 77) (cons "k" 19) (cons "F" 43) (cons "J" 4) (cons "U" 86) (cons "x" 12) (cons "S" 6) (cons "M" 31) (cons "D" 17) (cons "r" 13) (cons "Z" 31) (cons "z" 74) (cons "R" 66) (cons "w" 20) (cons "f" 85) (cons "d" 70) (cons "u" 66) (cons "l" 38) (cons "m" 51) (cons "H" 74) (cons "K" 8) (cons "A" 95) (cons "Q" 49) (cons "Y" 58) (cons "C" 88) (cons "I" 57) (cons "e" 10) (cons "y" 5) (cons "P" 11) (cons "p" 50) (cons "q" 11) (cons "o" 38) (cons "c" 0) (cons "b" 48) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12538 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "o" 5) (cons "p" 93) (cons "n" 33) (cons "K" 41) (cons "P" 21) (cons "f" 3) (cons "u" 97) (cons "d" 59) (cons "t" 20) (cons "Z" 89) (cons "z" 52) (cons "V" 24) (cons "L" 95) (cons "A" 23) (cons "a" 20) (cons "m" 65) (cons "v" 87) (cons "r" 20) (cons "G" 25) (cons "w" 33) (cons "R" 82) (cons "B" 44) (cons "X" 91) (cons "H" 33) (cons "F" 17) (cons "M" 42) (cons "l" 44) (cons "D" 55) (cons "C" 95) (cons "b" 65) (cons "E" 49) (cons "k" 54) (cons "T" 21) (cons "e" 48) (cons "O" 9) (cons "Q" 86) (cons "x" 54) (cons "U" 35) (cons "y" 97) (cons "Y" 59) (cons "i" 34) (cons "J" 88) (cons "g" 25) (cons "c" 37) (cons "W" 85) (cons "I" 18) (cons "j" 82) (cons "h" 90) (cons "s" 41) (cons "S" 77) (cons "N" 22) (cons "q" 80) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14595 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "P" 38) (cons "S" 82) (cons "C" 38) (cons "l" 62) (cons "o" 9) (cons "W" 18) (cons "G" 36) (cons "n" 34) (cons "F" 89) (cons "D" 73) (cons "q" 65) (cons "J" 65) (cons "A" 45) (cons "R" 94) (cons "f" 98) (cons "I" 65) (cons "Y" 7) (cons "y" 98) (cons "e" 55) (cons "Z" 60) (cons "t" 27) (cons "r" 93) (cons "N" 87) (cons "a" 69) (cons "M" 92) (cons "k" 41) (cons "X" 35) (cons "j" 35) (cons "c" 37) (cons "U" 75) (cons "Q" 92) (cons "O" 0) (cons "b" 62) (cons "V" 13) (cons "s" 41) (cons "K" 5) (cons "x" 90) (cons "v" 13) (cons "B" 22) (cons "E" 5) (cons "T" 84) (cons "H" 67) (cons "z" 4) (cons "d" 5) (cons "L" 56) (cons "u" 68) (cons "h" 24) (cons "g" 12) (cons "m" 2) (cons "p" 16) (cons "i" 31) (cons "w" 16) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13146 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "K" 10) (cons "J" 94) (cons "G" 74) (cons "A" 59) (cons "e" 0) (cons "x" 92) (cons "U" 7) (cons "d" 57) (cons "i" 89) (cons "w" 13) (cons "f" 88) (cons "l" 6) (cons "V" 17) (cons "O" 45) (cons "z" 94) (cons "h" 27) (cons "D" 42) (cons "N" 4) (cons "n" 69) (cons "Y" 80) (cons "I" 31) (cons "T" 84) (cons "v" 92) (cons "g" 92) (cons "j" 84) (cons "E" 28) (cons "b" 23) (cons "k" 87) (cons "Z" 11) (cons "X" 69) (cons "q" 78) (cons "Q" 37) (cons "C" 54) (cons "P" 28) (cons "s" 56) (cons "m" 84) (cons "r" 94) (cons "a" 29) (cons "u" 48) (cons "F" 74) (cons "o" 13) (cons "y" 25) (cons "p" 85) (cons "R" 34) (cons "H" 16) (cons "M" 47) (cons "t" 26) (cons "c" 34) (cons "B" 6) (cons "L" 89) (cons "W" 2) (cons "S" 3) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13559 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "O" 73) (cons "b" 38) (cons "Q" 12) (cons "d" 38) (cons "u" 84) (cons "q" 64) (cons "y" 57) (cons "k" 43) (cons "z" 8) (cons "s" 80) (cons "S" 95) (cons "C" 77) (cons "J" 26) (cons "o" 13) (cons "M" 65) (cons "L" 16) (cons "X" 49) (cons "n" 88) (cons "D" 16) (cons "H" 8) (cons "c" 87) (cons "V" 78) (cons "e" 82) (cons "v" 66) (cons "j" 70) (cons "p" 22) (cons "m" 62) (cons "P" 70) (cons "N" 77) (cons "E" 40) (cons "I" 58) (cons "f" 44) (cons "r" 31) (cons "W" 8) (cons "x" 26) (cons "g" 33) (cons "G" 49) (cons "U" 59) (cons "h" 73) (cons "a" 23) (cons "T" 89) (cons "F" 39) (cons "l" 18) (cons "K" 99) (cons "w" 23) (cons "Z" 61) (cons "R" 2) (cons "B" 58) (cons "A" 35) (cons "Y" 48) (cons "i" 38) (cons "t" 58) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14155 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "H" 15) (cons "F" 71) (cons "N" 14) (cons "w" 28) (cons "G" 17) (cons "L" 33) (cons "Y" 8) (cons "A" 77) (cons "Z" 88) (cons "t" 98) (cons "x" 74) (cons "h" 31) (cons "u" 79) (cons "o" 93) (cons "q" 30) (cons "b" 33) (cons "a" 1) (cons "T" 77) (cons "J" 77) (cons "z" 16) (cons "i" 2) (cons "R" 97) (cons "k" 63) (cons "n" 14) (cons "C" 70) (cons "D" 27) (cons "g" 83) (cons "f" 4) (cons "s" 48) (cons "X" 5) (cons "c" 24) (cons "U" 47) (cons "W" 14) (cons "p" 41) (cons "y" 39) (cons "j" 39) (cons "M" 97) (cons "e" 12) (cons "I" 52) (cons "V" 1) (cons "Q" 78) (cons "d" 94) (cons "O" 14) (cons "P" 81) (cons "S" 44) (cons "K" 92) (cons "v" 59) (cons "l" 48) (cons "E" 76) (cons "m" 82) (cons "B" 75) (cons "r" 60) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13673 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "i" 87) (cons "S" 20) (cons "f" 96) (cons "t" 11) (cons "Z" 95) (cons "z" 24) (cons "u" 28) (cons "V" 40) (cons "c" 53) (cons "y" 77) (cons "R" 54) (cons "x" 91) (cons "m" 33) (cons "J" 88) (cons "D" 74) (cons "W" 44) (cons "X" 8) (cons "M" 44) (cons "v" 41) (cons "n" 34) (cons "F" 3) (cons "e" 43) (cons "d" 12) (cons "h" 98) (cons "s" 87) (cons "E" 77) (cons "K" 78) (cons "G" 43) (cons "j" 85) (cons "I" 13) (cons "A" 51) (cons "L" 90) (cons "k" 80) (cons "q" 24) (cons "N" 74) (cons "O" 82) (cons "U" 6) (cons "Q" 57) (cons "T" 88) (cons "p" 30) (cons "r" 36) (cons "H" 2) (cons "a" 6) (cons "b" 67) (cons "o" 97) (cons "B" 54) (cons "g" 43) (cons "Y" 77) (cons "C" 30) (cons "P" 14) (cons "l" 12) (cons "w" 90) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14597 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "A" 7) (cons "o" 68) (cons "C" 5) (cons "T" 56) (cons "I" 13) (cons "y" 85) (cons "m" 14) (cons "E" 75) (cons "t" 44) (cons "h" 15) (cons "q" 98) (cons "b" 25) (cons "v" 4) (cons "S" 67) (cons "u" 52) (cons "L" 10) (cons "K" 84) (cons "G" 21) (cons "N" 71) (cons "n" 35) (cons "W" 71) (cons "i" 76) (cons "g" 10) (cons "z" 64) (cons "M" 65) (cons "R" 28) (cons "p" 77) (cons "f" 65) (cons "k" 6) (cons "s" 42) (cons "F" 0) (cons "O" 83) (cons "Q" 60) (cons "U" 62) (cons "e" 1) (cons "l" 26) (cons "H" 13) (cons "w" 83) (cons "B" 40) (cons "V" 67) (cons "c" 75) (cons "d" 21) (cons "a" 27) (cons "r" 85) (cons "x" 6) (cons "X" 86) (cons "P" 57) (cons "D" 83) (cons "Z" 70) (cons "Y" 28) (cons "j" 8) (cons "J" 70) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12891 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "X" 38) (cons "V" 65) (cons "E" 34) (cons "b" 7) (cons "s" 16) (cons "T" 85) (cons "q" 15) (cons "G" 75) (cons "i" 61) (cons "d" 14) (cons "P" 41) (cons "h" 86) (cons "Z" 69) (cons "j" 35) (cons "c" 51) (cons "M" 29) (cons "l" 0) (cons "J" 82) (cons "g" 70) (cons "B" 94) (cons "e" 16) (cons "m" 33) (cons "C" 16) (cons "S" 85) (cons "D" 21) (cons "Y" 90) (cons "K" 0) (cons "U" 97) (cons "p" 94) (cons "Q" 5) (cons "f" 95) (cons "n" 4) (cons "R" 14) (cons "a" 39) (cons "k" 21) (cons "u" 48) (cons "t" 91) (cons "x" 79) (cons "O" 61) (cons "y" 32) (cons "W" 95) (cons "v" 7) (cons "N" 57) (cons "H" 41) (cons "r" 68) (cons "A" 87) (cons "w" 28) (cons "I" 78) (cons "L" 3) (cons "F" 28) (cons "o" 44) (cons "z" 86) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13576 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "l" 4) (cons "b" 81) (cons "f" 71) (cons "e" 71) (cons "W" 79) (cons "Q" 79) (cons "p" 90) (cons "y" 80) (cons "I" 53) (cons "m" 35) (cons "N" 80) (cons "H" 41) (cons "X" 42) (cons "A" 51) (cons "U" 11) (cons "L" 73) (cons "B" 13) (cons "g" 13) (cons "G" 32) (cons "w" 74) (cons "Z" 48) (cons "V" 20) (cons "K" 60) (cons "u" 31) (cons "j" 77) (cons "o" 0) (cons "i" 72) (cons "M" 93) (cons "P" 2) (cons "v" 8) (cons "s" 73) (cons "r" 13) (cons "D" 53) (cons "c" 48) (cons "C" 95) (cons "k" 27) (cons "d" 79) (cons "z" 86) (cons "t" 93) (cons "h" 13) (cons "E" 34) (cons "Y" 57) (cons "J" 25) (cons "R" 45) (cons "O" 1) (cons "T" 75) (cons "F" 99) (cons "a" 37) (cons "n" 42) (cons "S" 55) (cons "q" 97) (cons "x" 1) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14218 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "t" 0) (cons "j" 86) (cons "u" 38) (cons "r" 27) (cons "W" 22) (cons "N" 17) (cons "M" 60) (cons "n" 33) (cons "G" 4) (cons "y" 5) (cons "T" 12) (cons "h" 61) (cons "Q" 20) (cons "H" 65) (cons "X" 59) (cons "D" 89) (cons "x" 59) (cons "C" 30) (cons "J" 18) (cons "L" 55) (cons "i" 43) (cons "U" 56) (cons "v" 38) (cons "A" 6) (cons "B" 14) (cons "P" 79) (cons "w" 31) (cons "a" 60) (cons "s" 16) (cons "b" 80) (cons "E" 26) (cons "e" 69) (cons "o" 31) (cons "p" 51) (cons "Z" 17) (cons "l" 72) (cons "d" 72) (cons "K" 1) (cons "O" 41) (cons "f" 80) (cons "S" 59) (cons "V" 37) (cons "k" 64) (cons "R" 71) (cons "F" 18) (cons "I" 28) (cons "Y" 37) (cons "m" 16) (cons "q" 69) (cons "c" 66) (cons "z" 15) (cons "g" 4) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 11487 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "T" 79) (cons "w" 98) (cons "B" 34) (cons "j" 40) (cons "X" 68) (cons "N" 30) (cons "R" 37) (cons "O" 94) (cons "q" 36) (cons "p" 58) (cons "L" 48) (cons "E" 20) (cons "i" 99) (cons "a" 54) (cons "k" 45) (cons "K" 19) (cons "A" 22) (cons "e" 80) (cons "Q" 66) (cons "x" 81) (cons "V" 96) (cons "c" 71) (cons "P" 59) (cons "M" 97) (cons "b" 72) (cons "U" 22) (cons "J" 46) (cons "W" 82) (cons "F" 93) (cons "l" 36) (cons "y" 29) (cons "n" 77) (cons "v" 62) (cons "g" 18) (cons "Z" 57) (cons "G" 48) (cons "h" 75) (cons "u" 96) (cons "H" 90) (cons "t" 71) (cons "z" 80) (cons "m" 56) (cons "Y" 90) (cons "r" 63) (cons "o" 88) (cons "f" 6) (cons "I" 18) (cons "C" 76) (cons "D" 0) (cons "S" 43) (cons "s" 20) (cons "d" 10) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 16314 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "q" 29) (cons "a" 25) (cons "D" 46) (cons "j" 69) (cons "z" 54) (cons "S" 35) (cons "P" 23) (cons "v" 13) (cons "w" 72) (cons "l" 19) (cons "t" 73) (cons "E" 33) (cons "O" 33) (cons "s" 10) (cons "T" 98) (cons "p" 98) (cons "K" 1) (cons "o" 13) (cons "u" 74) (cons "x" 95) (cons "H" 64) (cons "F" 13) (cons "M" 9) (cons "B" 34) (cons "y" 10) (cons "g" 36) (cons "k" 57) (cons "G" 68) (cons "I" 48) (cons "L" 30) (cons "N" 97) (cons "f" 36) (cons "R" 35) (cons "Z" 69) (cons "Y" 46) (cons "C" 72) (cons "r" 98) (cons "V" 88) (cons "n" 92) (cons "i" 45) (cons "e" 80) (cons "J" 25) (cons "b" 83) (cons "m" 83) (cons "U" 64) (cons "A" 9) (cons "X" 85) (cons "c" 10) (cons "W" 43) (cons "Q" 63) (cons "h" 1) (cons "d" 24) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13719 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "k" 59) (cons "v" 90) (cons "C" 83) (cons "h" 33) (cons "e" 39) (cons "P" 53) (cons "E" 36) (cons "M" 15) (cons "c" 19) (cons "z" 7) (cons "F" 17) (cons "m" 42) (cons "d" 13) (cons "D" 74) (cons "u" 82) (cons "Z" 69) (cons "O" 83) (cons "n" 85) (cons "G" 25) (cons "b" 87) (cons "A" 87) (cons "U" 48) (cons "x" 61) (cons "p" 31) (cons "N" 97) (cons "i" 34) (cons "R" 50) (cons "Y" 1) (cons "w" 88) (cons "W" 44) (cons "t" 40) (cons "y" 63) (cons "L" 82) (cons "J" 14) (cons "Q" 42) (cons "r" 64) (cons "l" 5) (cons "s" 12) (cons "a" 44) (cons "I" 76) (cons "H" 26) (cons "V" 21) (cons "X" 23) (cons "K" 83) (cons "q" 88) (cons "o" 89) (cons "f" 36) (cons "S" 96) (cons "j" 13) (cons "T" 80) (cons "g" 74) (cons "B" 52) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14610 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "U" 75) (cons "J" 85) (cons "u" 92) (cons "T" 18) (cons "w" 11) (cons "q" 58) (cons "k" 71) (cons "o" 89) (cons "I" 10) (cons "W" 91) (cons "i" 20) (cons "D" 32) (cons "j" 82) (cons "P" 2) (cons "R" 86) (cons "c" 15) (cons "z" 13) (cons "m" 84) (cons "p" 71) (cons "O" 65) (cons "y" 2) (cons "s" 38) (cons "l" 48) (cons "X" 90) (cons "Q" 28) (cons "Z" 96) (cons "E" 84) (cons "f" 53) (cons "F" 30) (cons "e" 76) (cons "v" 21) (cons "H" 25) (cons "n" 61) (cons "A" 85) (cons "r" 31) (cons "x" 42) (cons "V" 67) (cons "t" 74) (cons "b" 39) (cons "K" 29) (cons "h" 87) (cons "Y" 6) (cons "B" 76) (cons "a" 90) (cons "M" 34) (cons "S" 28) (cons "N" 73) (cons "d" 79) (cons "g" 59) (cons "G" 17) (cons "C" 67) (cons "L" 50) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 15056 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "W" 9) (cons "j" 43) (cons "o" 49) (cons "F" 33) (cons "q" 83) (cons "a" 25) (cons "r" 20) (cons "l" 48) (cons "t" 95) (cons "D" 61) (cons "H" 2) (cons "s" 95) (cons "L" 6) (cons "c" 83) (cons "U" 13) (cons "T" 47) (cons "y" 86) (cons "G" 34) (cons "I" 35) (cons "Y" 36) (cons "h" 60) (cons "x" 40) (cons "k" 52) (cons "Z" 15) (cons "g" 86) (cons "J" 67) (cons "E" 37) (cons "f" 98) (cons "K" 45) (cons "Q" 90) (cons "X" 70) (cons "w" 86) (cons "C" 5) (cons "R" 95) (cons "e" 99) (cons "B" 68) (cons "z" 58) (cons "n" 70) (cons "v" 46) (cons "V" 18) (cons "i" 4) (cons "N" 37) (cons "P" 25) (cons "u" 44) (cons "d" 70) (cons "O" 45) (cons "A" 38) (cons "S" 3) (cons "p" 67) (cons "m" 7) (cons "b" 79) (cons "M" 41) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13955 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "Q" 38) (cons "f" 8) (cons "Z" 6) (cons "a" 39) (cons "v" 57) (cons "P" 85) (cons "i" 36) (cons "e" 9) (cons "n" 96) (cons "B" 84) (cons "I" 82) (cons "b" 92) (cons "y" 43) (cons "p" 57) (cons "Y" 84) (cons "k" 94) (cons "c" 86) (cons "R" 57) (cons "K" 40) (cons "N" 84) (cons "m" 23) (cons "X" 58) (cons "d" 34) (cons "T" 92) (cons "D" 20) (cons "u" 15) (cons "C" 83) (cons "J" 77) (cons "q" 24) (cons "g" 46) (cons "F" 79) (cons "S" 31) (cons "w" 0) (cons "W" 93) (cons "l" 64) (cons "h" 4) (cons "A" 28) (cons "L" 89) (cons "M" 17) (cons "E" 69) (cons "j" 85) (cons "r" 10) (cons "t" 54) (cons "H" 43) (cons "U" 62) (cons "o" 30) (cons "x" 36) (cons "z" 58) (cons "V" 50) (cons "O" 24) (cons "s" 77) (cons "G" 38) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14690 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "n" 42) (cons "e" 69) (cons "Y" 96) (cons "r" 62) (cons "J" 99) (cons "p" 0) (cons "Z" 53) (cons "a" 55) (cons "j" 49) (cons "y" 72) (cons "h" 41) (cons "H" 35) (cons "g" 11) (cons "P" 35) (cons "f" 73) (cons "s" 53) (cons "F" 72) (cons "K" 60) (cons "A" 46) (cons "M" 84) (cons "G" 90) (cons "i" 77) (cons "q" 53) (cons "m" 48) (cons "b" 26) (cons "U" 23) (cons "X" 37) (cons "V" 18) (cons "T" 13) (cons "Q" 8) (cons "c" 15) (cons "B" 3) (cons "W" 65) (cons "I" 49) (cons "o" 82) (cons "D" 21) (cons "C" 77) (cons "N" 18) (cons "R" 19) (cons "S" 60) (cons "z" 30) (cons "u" 27) (cons "E" 11) (cons "l" 71) (cons "t" 99) (cons "x" 41) (cons "v" 36) (cons "O" 17) (cons "w" 22) (cons "L" 20) (cons "k" 31) (cons "d" 67) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12999 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "p" 35) (cons "l" 88) (cons "B" 37) (cons "U" 2) (cons "d" 29) (cons "y" 10) (cons "w" 83) (cons "o" 19) (cons "q" 9) (cons "g" 98) (cons "x" 22) (cons "T" 49) (cons "h" 13) (cons "m" 25) (cons "k" 54) (cons "O" 73) (cons "c" 41) (cons "j" 65) (cons "i" 80) (cons "S" 79) (cons "z" 14) (cons "a" 77) (cons "v" 83) (cons "s" 16) (cons "X" 8) (cons "R" 58) (cons "G" 58) (cons "K" 24) (cons "I" 55) (cons "L" 7) (cons "W" 14) (cons "F" 0) (cons "D" 89) (cons "H" 90) (cons "Y" 26) (cons "e" 80) (cons "t" 0) (cons "b" 83) (cons "C" 2) (cons "A" 3) (cons "Z" 50) (cons "J" 57) (cons "N" 60) (cons "n" 1) (cons "r" 64) (cons "P" 91) (cons "Q" 85) (cons "f" 24) (cons "M" 82) (cons "E" 15) (cons "u" 19) (cons "V" 51) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12193 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "L" 29) (cons "B" 34) (cons "o" 75) (cons "t" 24) (cons "c" 73) (cons "e" 61) (cons "R" 73) (cons "V" 10) (cons "T" 27) (cons "E" 47) (cons "x" 53) (cons "g" 8) (cons "i" 64) (cons "r" 57) (cons "M" 33) (cons "l" 66) (cons "d" 97) (cons "a" 67) (cons "W" 95) (cons "q" 33) (cons "C" 88) (cons "I" 54) (cons "y" 83) (cons "n" 92) (cons "G" 35) (cons "S" 29) (cons "b" 4) (cons "K" 32) (cons "D" 17) (cons "P" 21) (cons "u" 74) (cons "H" 14) (cons "j" 26) (cons "N" 70) (cons "m" 7) (cons "J" 75) (cons "X" 24) (cons "p" 4) (cons "f" 50) (cons "k" 95) (cons "U" 98) (cons "w" 32) (cons "A" 5) (cons "Q" 41) (cons "Z" 65) (cons "h" 5) (cons "O" 86) (cons "F" 71) (cons "s" 3) (cons "z" 9) (cons "Y" 5) (cons "v" 22) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12698 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "T" 51) (cons "w" 10) (cons "N" 81) (cons "X" 8) (cons "n" 19) (cons "K" 73) (cons "Z" 97) (cons "m" 34) (cons "D" 88) (cons "C" 26) (cons "Y" 69) (cons "z" 37) (cons "B" 97) (cons "P" 58) (cons "x" 57) (cons "A" 83) (cons "p" 25) (cons "W" 2) (cons "U" 16) (cons "f" 16) (cons "V" 22) (cons "s" 52) (cons "G" 41) (cons "y" 72) (cons "M" 80) (cons "g" 83) (cons "i" 89) (cons "H" 50) (cons "L" 41) (cons "l" 89) (cons "Q" 74) (cons "R" 17) (cons "r" 62) (cons "k" 1) (cons "a" 60) (cons "O" 29) (cons "j" 35) (cons "q" 95) (cons "t" 27) (cons "o" 99) (cons "h" 34) (cons "b" 79) (cons "u" 16) (cons "I" 82) (cons "c" 67) (cons "F" 56) (cons "E" 9) (cons "e" 51) (cons "v" 13) (cons "S" 44) (cons "d" 37) (cons "J" 19) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14001 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "O" 78) (cons "r" 86) (cons "a" 69) (cons "t" 99) (cons "H" 50) (cons "q" 4) (cons "M" 9) (cons "l" 17) (cons "p" 85) (cons "T" 19) (cons "y" 13) (cons "R" 2) (cons "z" 93) (cons "b" 65) (cons "j" 57) (cons "h" 57) (cons "w" 3) (cons "U" 65) (cons "n" 65) (cons "I" 2) (cons "K" 82) (cons "x" 55) (cons "C" 21) (cons "c" 78) (cons "L" 78) (cons "e" 36) (cons "f" 62) (cons "u" 85) (cons "S" 9) (cons "D" 70) (cons "Q" 2) (cons "o" 27) (cons "P" 36) (cons "W" 64) (cons "k" 55) (cons "Y" 30) (cons "m" 97) (cons "G" 34) (cons "V" 43) (cons "Z" 94) (cons "v" 74) (cons "J" 26) (cons "X" 55) (cons "g" 80) (cons "B" 93) (cons "d" 98) (cons "i" 20) (cons "s" 77) (cons "A" 55) (cons "N" 51) (cons "F" 5) (cons "E" 3) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14192 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "p" 18) (cons "b" 22) (cons "m" 30) (cons "J" 16) (cons "u" 88) (cons "U" 26) (cons "S" 74) (cons "s" 23) (cons "N" 90) (cons "W" 52) (cons "A" 82) (cons "Z" 99) (cons "e" 8) (cons "l" 39) (cons "O" 48) (cons "y" 14) (cons "H" 73) (cons "w" 52) (cons "f" 17) (cons "j" 6) (cons "C" 12) (cons "q" 67) (cons "V" 36) (cons "d" 37) (cons "x" 85) (cons "t" 31) (cons "n" 89) (cons "G" 63) (cons "P" 91) (cons "I" 30) (cons "F" 37) (cons "K" 25) (cons "L" 22) (cons "Q" 9) (cons "z" 68) (cons "B" 88) (cons "X" 36) (cons "v" 16) (cons "o" 80) (cons "r" 14) (cons "a" 16) (cons "c" 69) (cons "R" 48) (cons "i" 42) (cons "g" 55) (cons "h" 26) (cons "E" 66) (cons "T" 99) (cons "k" 36) (cons "D" 97) (cons "M" 61) (cons "Y" 41) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13477 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "t" 25) (cons "f" 58) (cons "c" 52) (cons "K" 6) (cons "Z" 66) (cons "m" 57) (cons "d" 66) (cons "x" 86) (cons "h" 94) (cons "l" 29) (cons "e" 38) (cons "z" 69) (cons "Q" 10) (cons "k" 20) (cons "F" 61) (cons "G" 18) (cons "a" 16) (cons "D" 26) (cons "H" 45) (cons "s" 99) (cons "q" 48) (cons "A" 18) (cons "I" 55) (cons "n" 23) (cons "J" 52) (cons "u" 84) (cons "C" 1) (cons "y" 9) (cons "g" 9) (cons "i" 64) (cons "E" 14) (cons "O" 62) (cons "p" 82) (cons "R" 77) (cons "B" 91) (cons "P" 52) (cons "L" 71) (cons "S" 99) (cons "w" 71) (cons "U" 55) (cons "Y" 54) (cons "o" 63) (cons "W" 62) (cons "v" 4) (cons "j" 6) (cons "r" 15) (cons "M" 42) (cons "b" 94) (cons "T" 97) (cons "X" 66) (cons "V" 58) (cons "N" 27) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13978 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "J" 87) (cons "M" 28) (cons "Q" 37) (cons "X" 52) (cons "r" 52) (cons "O" 55) (cons "G" 37) (cons "c" 68) (cons "y" 36) (cons "N" 70) (cons "F" 11) (cons "a" 73) (cons "j" 67) (cons "R" 68) (cons "S" 15) (cons "l" 4) (cons "T" 37) (cons "H" 88) (cons "D" 60) (cons "Z" 96) (cons "t" 3) (cons "m" 11) (cons "A" 24) (cons "i" 22) (cons "q" 76) (cons "B" 41) (cons "n" 53) (cons "I" 79) (cons "f" 23) (cons "z" 92) (cons "v" 92) (cons "x" 70) (cons "C" 16) (cons "b" 88) (cons "u" 83) (cons "Y" 82) (cons "o" 57) (cons "d" 19) (cons "e" 0) (cons "g" 55) (cons "s" 13) (cons "L" 74) (cons "K" 45) (cons "E" 32) (cons "p" 82) (cons "W" 35) (cons "U" 41) (cons "P" 52) (cons "h" 42) (cons "k" 47) (cons "w" 76) (cons "V" 77) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14479 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "p" 27) (cons "y" 62) (cons "L" 82) (cons "x" 7) (cons "N" 38) (cons "n" 86) (cons "C" 26) (cons "w" 31) (cons "S" 18) (cons "j" 51) (cons "W" 68) (cons "r" 34) (cons "U" 36) (cons "u" 31) (cons "g" 28) (cons "f" 7) (cons "H" 55) (cons "i" 63) (cons "R" 47) (cons "t" 46) (cons "h" 12) (cons "b" 43) (cons "K" 69) (cons "l" 60) (cons "a" 49) (cons "q" 93) (cons "o" 21) (cons "m" 35) (cons "e" 12) (cons "V" 36) (cons "Y" 99) (cons "P" 99) (cons "T" 24) (cons "Z" 38) (cons "d" 24) (cons "E" 34) (cons "X" 20) (cons "Q" 89) (cons "A" 61) (cons "F" 82) (cons "z" 96) (cons "I" 75) (cons "v" 85) (cons "M" 73) (cons "D" 53) (cons "k" 11) (cons "J" 0) (cons "O" 22) (cons "s" 40) (cons "B" 59) (cons "G" 50) (cons "c" 87) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13662 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "G" 39) (cons "l" 49) (cons "D" 64) (cons "z" 97) (cons "R" 74) (cons "U" 94) (cons "n" 50) (cons "S" 23) (cons "x" 87) (cons "i" 28) (cons "b" 88) (cons "h" 37) (cons "q" 37) (cons "Y" 29) (cons "y" 15) (cons "g" 44) (cons "B" 17) (cons "u" 8) (cons "E" 3) (cons "K" 81) (cons "c" 4) (cons "I" 99) (cons "O" 45) (cons "V" 49) (cons "N" 89) (cons "Q" 59) (cons "Z" 20) (cons "e" 66) (cons "s" 46) (cons "t" 79) (cons "j" 63) (cons "J" 86) (cons "H" 14) (cons "F" 21) (cons "P" 17) (cons "p" 25) (cons "m" 42) (cons "o" 23) (cons "L" 14) (cons "r" 5) (cons "A" 34) (cons "a" 85) (cons "d" 53) (cons "W" 30) (cons "v" 69) (cons "T" 61) (cons "X" 53) (cons "C" 8) (cons "f" 76) (cons "w" 86) (cons "M" 49) (cons "k" 92) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13760 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "u" 73) (cons "M" 2) (cons "V" 65) (cons "p" 23) (cons "x" 55) (cons "y" 92) (cons "A" 14) (cons "m" 73) (cons "L" 26) (cons "Y" 44) (cons "r" 95) (cons "c" 45) (cons "B" 95) (cons "z" 1) (cons "b" 34) (cons "t" 70) (cons "i" 4) (cons "O" 20) (cons "H" 1) (cons "g" 30) (cons "R" 78) (cons "F" 39) (cons "K" 60) (cons "o" 81) (cons "h" 89) (cons "n" 52) (cons "e" 39) (cons "C" 69) (cons "q" 36) (cons "d" 7) (cons "j" 15) (cons "P" 25) (cons "k" 42) (cons "U" 68) (cons "I" 29) (cons "v" 67) (cons "f" 60) (cons "Q" 36) (cons "G" 46) (cons "l" 97) (cons "X" 52) (cons "S" 90) (cons "w" 79) (cons "D" 60) (cons "E" 28) (cons "W" 70) (cons "T" 1) (cons "J" 73) (cons "Z" 5) (cons "a" 74) (cons "s" 70) (cons "N" 18) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13625 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "v" 85) (cons "J" 77) (cons "P" 46) (cons "u" 6) (cons "r" 22) (cons "z" 74) (cons "R" 20) (cons "k" 64) (cons "w" 37) (cons "a" 79) (cons "m" 58) (cons "D" 90) (cons "H" 34) (cons "x" 51) (cons "U" 63) (cons "Q" 51) (cons "n" 44) (cons "A" 98) (cons "Z" 47) (cons "g" 91) (cons "V" 55) (cons "e" 36) (cons "T" 40) (cons "C" 48) (cons "G" 34) (cons "t" 5) (cons "q" 41) (cons "b" 49) (cons "h" 41) (cons "N" 27) (cons "y" 0) (cons "W" 21) (cons "E" 7) (cons "p" 52) (cons "o" 75) (cons "S" 70) (cons "B" 70) (cons "c" 99) (cons "I" 13) (cons "M" 34) (cons "K" 26) (cons "X" 94) (cons "i" 53) (cons "f" 19) (cons "Y" 22) (cons "L" 49) (cons "s" 53) (cons "d" 44) (cons "l" 8) (cons "O" 92) (cons "F" 19) (cons "j" 32) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13510 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "V" 66) (cons "n" 63) (cons "T" 22) (cons "s" 4) (cons "D" 84) (cons "X" 52) (cons "B" 65) (cons "N" 44) (cons "x" 40) (cons "W" 93) (cons "Y" 48) (cons "R" 45) (cons "f" 73) (cons "k" 35) (cons "v" 11) (cons "l" 49) (cons "r" 30) (cons "G" 75) (cons "w" 90) (cons "K" 62) (cons "y" 72) (cons "Z" 62) (cons "t" 60) (cons "e" 94) (cons "H" 65) (cons "E" 89) (cons "P" 18) (cons "m" 33) (cons "M" 99) (cons "j" 89) (cons "I" 80) (cons "F" 4) (cons "O" 79) (cons "b" 71) (cons "o" 26) (cons "c" 52) (cons "a" 72) (cons "g" 91) (cons "p" 32) (cons "z" 42) (cons "L" 60) (cons "q" 22) (cons "U" 38) (cons "Q" 57) (cons "S" 1) (cons "C" 64) (cons "u" 67) (cons "A" 3) (cons "d" 82) (cons "h" 81) (cons "i" 72) (cons "J" 19) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 15702 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "A" 49) (cons "V" 11) (cons "n" 29) (cons "y" 39) (cons "N" 62) (cons "a" 63) (cons "T" 93) (cons "g" 58) (cons "i" 49) (cons "s" 58) (cons "v" 85) (cons "u" 13) (cons "L" 24) (cons "q" 90) (cons "H" 94) (cons "X" 69) (cons "j" 19) (cons "p" 18) (cons "k" 43) (cons "F" 44) (cons "t" 82) (cons "J" 29) (cons "r" 59) (cons "d" 99) (cons "E" 46) (cons "Q" 4) (cons "f" 98) (cons "b" 95) (cons "z" 11) (cons "G" 25) (cons "R" 77) (cons "c" 3) (cons "P" 77) (cons "w" 69) (cons "Z" 35) (cons "M" 87) (cons "C" 49) (cons "D" 49) (cons "K" 10) (cons "x" 83) (cons "o" 60) (cons "e" 64) (cons "S" 64) (cons "O" 2) (cons "m" 29) (cons "Y" 25) (cons "I" 97) (cons "h" 7) (cons "l" 2) (cons "U" 26) (cons "B" 95) (cons "W" 31) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14088 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "q" 26) (cons "s" 80) (cons "R" 73) (cons "T" 13) (cons "H" 13) (cons "C" 97) (cons "o" 68) (cons "c" 11) (cons "G" 67) (cons "Z" 55) (cons "J" 50) (cons "X" 48) (cons "Q" 58) (cons "b" 9) (cons "V" 17) (cons "k" 62) (cons "Y" 83) (cons "z" 95) (cons "a" 18) (cons "f" 85) (cons "j" 92) (cons "w" 13) (cons "O" 95) (cons "y" 44) (cons "N" 13) (cons "W" 88) (cons "P" 80) (cons "t" 92) (cons "m" 84) (cons "F" 77) (cons "v" 87) (cons "I" 93) (cons "u" 2) (cons "i" 88) (cons "h" 31) (cons "e" 75) (cons "A" 16) (cons "L" 84) (cons "K" 36) (cons "S" 19) (cons "g" 63) (cons "B" 44) (cons "U" 98) (cons "n" 41) (cons "r" 55) (cons "x" 36) (cons "l" 71) (cons "E" 16) (cons "p" 94) (cons "M" 23) (cons "d" 94) (cons "D" 84) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 16175 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "w" 71) (cons "W" 19) (cons "a" 9) (cons "M" 73) (cons "g" 58) (cons "h" 95) (cons "e" 63) (cons "K" 78) (cons "Y" 45) (cons "k" 2) (cons "x" 65) (cons "S" 95) (cons "o" 36) (cons "s" 54) (cons "T" 88) (cons "m" 68) (cons "p" 46) (cons "G" 81) (cons "z" 46) (cons "I" 32) (cons "t" 71) (cons "q" 65) (cons "n" 86) (cons "c" 6) (cons "P" 38) (cons "E" 66) (cons "Q" 40) (cons "l" 3) (cons "j" 42) (cons "D" 50) (cons "X" 91) (cons "V" 60) (cons "H" 46) (cons "F" 85) (cons "N" 8) (cons "f" 42) (cons "O" 66) (cons "i" 25) (cons "Z" 24) (cons "y" 87) (cons "B" 3) (cons "L" 54) (cons "v" 48) (cons "U" 45) (cons "u" 28) (cons "b" 41) (cons "R" 38) (cons "r" 39) (cons "d" 28) (cons "C" 21) (cons "A" 31) (cons "J" 88) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14212 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "C" 87) (cons "l" 3) (cons "f" 14) (cons "K" 83) (cons "J" 56) (cons "s" 45) (cons "D" 82) (cons "v" 10) (cons "F" 42) (cons "g" 30) (cons "M" 58) (cons "n" 80) (cons "A" 6) (cons "W" 0) (cons "I" 73) (cons "c" 39) (cons "Y" 50) (cons "H" 15) (cons "a" 94) (cons "d" 41) (cons "Z" 90) (cons "q" 54) (cons "h" 88) (cons "N" 18) (cons "U" 28) (cons "m" 46) (cons "B" 65) (cons "T" 1) (cons "E" 19) (cons "o" 99) (cons "w" 98) (cons "L" 23) (cons "R" 74) (cons "t" 26) (cons "V" 82) (cons "Q" 0) (cons "j" 38) (cons "b" 15) (cons "x" 44) (cons "y" 37) (cons "u" 57) (cons "P" 29) (cons "k" 3) (cons "p" 54) (cons "S" 9) (cons "G" 97) (cons "i" 14) (cons "O" 72) (cons "z" 81) (cons "r" 95) (cons "e" 4) (cons "X" 7) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12647 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "M" 76) (cons "A" 82) (cons "U" 75) (cons "r" 57) (cons "B" 25) (cons "j" 6) (cons "e" 2) (cons "n" 84) (cons "I" 11) (cons "R" 20) (cons "i" 50) (cons "W" 69) (cons "S" 56) (cons "T" 8) (cons "l" 29) (cons "q" 20) (cons "J" 6) (cons "z" 21) (cons "K" 7) (cons "O" 8) (cons "v" 87) (cons "f" 2) (cons "g" 88) (cons "b" 14) (cons "X" 61) (cons "s" 32) (cons "H" 76) (cons "p" 14) (cons "k" 31) (cons "P" 84) (cons "E" 75) (cons "F" 48) (cons "G" 47) (cons "d" 95) (cons "w" 73) (cons "a" 29) (cons "m" 93) (cons "c" 24) (cons "y" 80) (cons "Q" 33) (cons "C" 20) (cons "u" 69) (cons "t" 14) (cons "L" 51) (cons "N" 84) (cons "h" 12) (cons "x" 30) (cons "Z" 47) (cons "D" 68) (cons "V" 40) (cons "o" 73) (cons "Y" 38) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12631 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "U" 6) (cons "h" 65) (cons "f" 50) (cons "i" 53) (cons "C" 72) (cons "G" 0) (cons "O" 66) (cons "F" 94) (cons "p" 98) (cons "s" 52) (cons "y" 74) (cons "R" 11) (cons "P" 95) (cons "T" 43) (cons "o" 28) (cons "J" 45) (cons "L" 84) (cons "t" 20) (cons "v" 71) (cons "A" 7) (cons "e" 8) (cons "H" 68) (cons "V" 24) (cons "a" 82) (cons "D" 25) (cons "Y" 53) (cons "Z" 93) (cons "q" 53) (cons "w" 41) (cons "Q" 26) (cons "K" 28) (cons "x" 72) (cons "z" 4) (cons "g" 69) (cons "E" 93) (cons "M" 37) (cons "X" 0) (cons "n" 87) (cons "u" 83) (cons "k" 44) (cons "l" 85) (cons "d" 79) (cons "j" 19) (cons "c" 39) (cons "W" 0) (cons "I" 0) (cons "B" 9) (cons "r" 90) (cons "b" 26) (cons "N" 37) (cons "S" 42) (cons "m" 36) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13340 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "B" 70) (cons "M" 24) (cons "b" 18) (cons "n" 95) (cons "L" 94) (cons "o" 75) (cons "Q" 48) (cons "X" 13) (cons "f" 25) (cons "F" 55) (cons "G" 90) (cons "A" 17) (cons "W" 41) (cons "O" 34) (cons "I" 47) (cons "s" 35) (cons "q" 22) (cons "v" 42) (cons "D" 69) (cons "H" 5) (cons "r" 79) (cons "K" 19) (cons "w" 78) (cons "a" 77) (cons "Z" 81) (cons "h" 45) (cons "i" 24) (cons "m" 93) (cons "N" 74) (cons "u" 73) (cons "p" 73) (cons "l" 46) (cons "P" 20) (cons "U" 9) (cons "j" 49) (cons "x" 91) (cons "J" 47) (cons "Y" 5) (cons "R" 18) (cons "k" 74) (cons "e" 42) (cons "T" 81) (cons "E" 41) (cons "C" 17) (cons "g" 48) (cons "c" 2) (cons "V" 21) (cons "d" 75) (cons "S" 57) (cons "t" 28) (cons "z" 82) (cons "y" 33) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13764 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "J" 65) (cons "X" 69) (cons "B" 74) (cons "g" 56) (cons "i" 23) (cons "x" 36) (cons "m" 3) (cons "M" 39) (cons "f" 15) (cons "c" 41) (cons "Z" 30) (cons "V" 54) (cons "k" 0) (cons "v" 80) (cons "t" 32) (cons "y" 12) (cons "S" 79) (cons "r" 35) (cons "o" 91) (cons "E" 59) (cons "P" 4) (cons "T" 24) (cons "Y" 67) (cons "N" 52) (cons "n" 89) (cons "L" 54) (cons "A" 29) (cons "w" 5) (cons "l" 20) (cons "W" 6) (cons "d" 57) (cons "q" 35) (cons "D" 53) (cons "U" 86) (cons "Q" 88) (cons "z" 54) (cons "F" 21) (cons "j" 54) (cons "a" 54) (cons "e" 52) (cons "b" 28) (cons "H" 38) (cons "R" 14) (cons "s" 11) (cons "h" 63) (cons "p" 24) (cons "I" 43) (cons "C" 33) (cons "G" 99) (cons "O" 66) (cons "K" 32) (cons "u" 1) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 12207 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "L" 64) (cons "v" 42) (cons "V" 69) (cons "J" 67) (cons "Y" 0) (cons "Z" 71) (cons "X" 88) (cons "K" 5) (cons "w" 87) (cons "l" 13) (cons "m" 40) (cons "E" 19) (cons "I" 4) (cons "M" 11) (cons "N" 27) (cons "t" 42) (cons "C" 81) (cons "c" 2) (cons "T" 9) (cons "n" 36) (cons "S" 62) (cons "h" 36) (cons "U" 81) (cons "x" 77) (cons "g" 58) (cons "A" 49) (cons "W" 27) (cons "R" 31) (cons "Q" 3) (cons "B" 94) (cons "e" 54) (cons "y" 41) (cons "f" 12) (cons "j" 51) (cons "H" 99) (cons "r" 95) (cons "s" 51) (cons "b" 95) (cons "a" 26) (cons "d" 36) (cons "q" 72) (cons "F" 87) (cons "p" 28) (cons "o" 24) (cons "O" 61) (cons "G" 22) (cons "D" 85) (cons "P" 5) (cons "z" 57) (cons "i" 37) (cons "u" 69) (cons "k" 62) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13319 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "t" 48) (cons "B" 11) (cons "S" 70) (cons "O" 63) (cons "h" 73) (cons "y" 46) (cons "J" 4) (cons "Y" 10) (cons "E" 94) (cons "A" 19) (cons "n" 53) (cons "m" 96) (cons "f" 76) (cons "W" 91) (cons "H" 93) (cons "T" 25) (cons "I" 27) (cons "R" 37) (cons "M" 74) (cons "L" 68) (cons "c" 9) (cons "x" 84) (cons "D" 37) (cons "X" 7) (cons "i" 1) (cons "l" 55) (cons "e" 36) (cons "C" 93) (cons "b" 69) (cons "G" 97) (cons "U" 65) (cons "k" 3) (cons "g" 62) (cons "w" 47) (cons "Z" 86) (cons "d" 43) (cons "a" 12) (cons "j" 65) (cons "Q" 8) (cons "q" 0) (cons "N" 17) (cons "p" 48) (cons "K" 49) (cons "V" 93) (cons "F" 26) (cons "z" 95) (cons "P" 48) (cons "r" 62) (cons "v" 88) (cons "s" 52) (cons "o" 90) (cons "u" 85) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 14661 score)))))

(sgoutput
 (lambda ()
  (let* ([frequencies (list (cons "M" 50) (cons "r" 4) (cons "F" 95) (cons "S" 37) (cons "J" 35) (cons "b" 7) (cons "e" 7) (cons "n" 70) (cons "K" 1) (cons "u" 99) (cons "E" 13) (cons "Z" 73) (cons "s" 83) (cons "N" 80) (cons "H" 52) (cons "P" 36) (cons "j" 4) (cons "w" 52) (cons "m" 79) (cons "z" 76) (cons "t" 20) (cons "h" 0) (cons "g" 55) (cons "a" 37) (cons "y" 17) (cons "p" 88) (cons "l" 80) (cons "d" 3) (cons "O" 81) (cons "T" 8) (cons "V" 77) (cons "v" 18) (cons "U" 88) (cons "I" 30) (cons "R" 80) (cons "x" 59) (cons "W" 95) (cons "L" 62) (cons "Q" 83) (cons "c" 53) (cons "q" 66) (cons "X" 17) (cons "i" 35) (cons "D" 33) (cons "Y" 44) (cons "G" 39) (cons "A" 19) (cons "f" 13) (cons "B" 53) (cons "o" 47) (cons "C" 83) (cons "k" 21) )]
         [codes (vlencode frequencies)]
         [wf? (wellformed? frequencies codes)]
         [score (compute-score frequencies codes)])
         (and wf? (equal? 13213 score)))))

(sgoutput
 (lambda ()
   (let* ([frequencies (list (cons "a" 5) (cons "b" 1) (cons "c" 1) (cons "d" 1))]
          [codes (vlencode frequencies)]
          [wf? (wellformed? frequencies codes)]
          [score (compute-score frequencies codes)])
     (and wf? (equal? 13 score)))))

(sgoutput
 (lambda ()
   (let* ([frequencies (list (cons "a" 3) (cons "b" 4) (cons "c" 5) (cons "d" 6))]
          [codes (vlencode frequencies)]
          [wf? (wellformed? frequencies codes)]
          [score (compute-score frequencies codes)])
     (and wf? (equal? 36 score)))))

(sgoutput
 (lambda ()
   (let* ([frequencies (list (cons "a" 3) (cons "b" 4) (cons "c" 5) (cons "d" 6) (cons "e" 0))]
          [codes (vlencode frequencies)]
          [wf? (wellformed? frequencies codes)]
          [score (compute-score frequencies codes)])
     (and wf? (equal? 36 score)))))

(sgoutput
 (lambda ()
   (let* ([frequencies '()]
          [codes (vlencode frequencies)]
          [wf? (wellformed? frequencies codes)]
          [score (compute-score frequencies codes)])
     (and wf? (equal? 0 score)))))
