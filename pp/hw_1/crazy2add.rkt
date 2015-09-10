#lang racket
(define p0 '(z z z . p))
(define p1 '(p . p))

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

(define (sum c0 c1 crry) ;sum of a digit
  (cond ((equal? (+ (val c0) (val c1) (val crry)) 3) '(p . p))
        ((equal? (+ (val c0) (val c1) (val crry)) 2) '(z . p))
        ((equal? (+ (val c0) (val c1) (val crry)) 1) '(p . z))
        ((equal? (+ (val c0) (val c1) (val crry)) 0) '(z . z))
        ((equal? (+ (val c0) (val c1) (val crry)) -1) '(n . z))
        ((equal? (+ (val c0) (val c1) (val crry)) -2) '(z . n))
        ((equal? (+ (val c0) (val c1) (val crry)) -3) '(n . n))))

(define (c2add c0 c1 crry)
  (cond  ((and (not (pair? c0)) (not (pair? c1))) 
          (sum c0 c1 crry));check
         ((and (not (pair? c0)) (pair? c1))
          (let ([p (sum c0 (car c1) crry)]) p
            (cons (car p) (c2add (cdr p)) (cdr c1) 0)))
         ((and (pair? c0) (not (pair? c1))) 
          (let ([p (sum c1 (car c0) crry)]) p
            (cons (car p) (c2add (cdr c0) (cdr p) 0))))
         (else 
           (let ([p (sum (car c1) (car c0) crry)]) p
             (cons (car p) (c2add (cdr c0) (cdr c1) (cdr p)))))))

(define (crazy2add c0 c1)
  ;carry is 0 at the beginning
  (c2add c0 c1 0))

(+ 1 2 3)

(crazy2add p0 p1)
(crazy2val (crazy2add p0 p1))
