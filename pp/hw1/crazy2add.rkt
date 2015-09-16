#lang racket
(define p0 '(z . z))
(define p1 '(z . z))

(define (val c)
  (cond ((equal? c 'z) 0)
        ((equal? c 'p) 1)
        ((equal? c 'n) -1)
        (else 0)));(val 0 is 0)

(define (crazy2val p)
  (if (pair? p)
    (+ (* (crazy2val (cdr p)) 2) (val (car p)))
    ;base case
    (val p)))

(define (sum c0 c1 crry) ;sum of a digit
  (cond ((equal? (+ (val c0) (val c1) (val crry)) 3) '(p . p))
        ((equal? (+ (val c0) (val c1) (val crry)) 2) '(z . p))
        ((equal? (+ (val c0) (val c1) (val crry)) 1) '(p . z))
        ((equal? (+ (val c0) (val c1) (val crry)) 0) '(z . z))
        ((equal? (+ (val c0) (val c1) (val crry)) -1) '(n . z))
        ((equal? (+ (val c0) (val c1) (val crry)) -2) '(z . n))
        ((equal? (+ (val c0) (val c1) (val crry)) -3) '(n . n))))

(define (f c)
    (if (pair? c)
      (car c)
      c));for convenience but not very type-safe?

(define (g c)
    (if (pair? c)
      (cdr c)
      0));for convenience

(define (c2add c0 c1 crry)
  (if (and (not (pair? c0)) (not (pair? c1)))
          (sum c0 c1 crry) ;base case
  (let ([p (sum (f c0) (f c1) crry)])
    (cons (car p) (c2add (g c0) (g c1) (cdr p))))))

(define (crazy2add c0 c1)
  ;;carry is 0 at the beginning
  (c2add c0 c1 0))

(crazy2add p0 p1)
(crazy2val (crazy2add p0 p1))
;(f '(z . p))
