#lang racket
(define p0 '(z n . p))
(define p1 '(n z . p))

(define (val c)
  (cond ((equal? c 'z) 0)
        ((equal? c 'p) 1)
        ((equal? c 'n) -1)
        (else 0)))

(define (crazy2val p)
  (if (pair? p)
    (+ (* (crazy2val (cdr p)) 2) (val (car p)))
    ;base case
    (val p)))

(define (sum c0 c1) ;sum of a digit
  (cond ((equal? (+ (val c0) (val c1)) 2) '(z . p))
        ((equal? (+ (val c0) (val c1)) 1) '(p . z))
        ((equal? (+ (val c0) (val c1)) 0) '(z . z))
        ((equal? (+ (val c0) (val c1)) -1) '(n . z))
        ((equal? (+ (val c0) (val c1)) -2) '(z . n))
        ((equal? (+ (val c0) (val c1)) 3) '(p . p));needed?
        ((equal? (+ (val c0) (val c1)) -3) '(n . n));needed?
        ))

;TODO : add carry as an argument?('z for start?) then sum gets three num also? or call sum twice?
;add one digit each
(define (c2add c0 c1)
  (cond  ((and (not (pair? c0)) (not (pair? c1))) (sum c0 c1));check
         ((and (not (pair? c0)) (pair? c1)) (cons (car (sum c0 (car c1))) (c2add (cdr (sum c0 (car c1))) (cdr c1))))
         ((and (pair? c0) (not (pair? c1))) (cons (car (sum (car c0) c1)) (c2add (cdr c0) (cdr (sum (car c0) c1)))))
         (else (cons (car (sum (car c0) (car c1))) (c2add (cdr c0) (cdr c1))))))


(c2add p0 p1)
(crazy2val (c2add p0 p1))
