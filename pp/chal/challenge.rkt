#lang racket
(provide myeval)
;env: list of hashtables
(define ERROR 'error)

;TODO: divide myeval in to modules
(define (myeval E env) ;myeval: (Expression) List -> E
  ;(write env)(newline)
  ;(write E)(newline)
  (if (equal? E ''())
    '()
    (if (or (number? E) (boolean? E))
      E
      (if (not (list? E))
        (let ()
          (look-up E env)) ;variable -> look up environment
        (let () ;composite expressions
          (define t (car E))
          (cond 
            ; ARITHMETIC - nothing but just application????
            ((equal? t '+) (+ (myeval (cadr E) env) (myeval (caddr E) env))) 
            ((equal? t '-) (- (myeval (cadr E) env) (myeval (caddr E) env)))
            ((equal? t '*) (* (myeval (cadr E) env) (myeval (caddr E) env)))
            ((equal? t '=) (= (myeval (cadr E) env) (myeval (caddr E) env)))
            ((equal? t '<) (< (myeval (cadr E) env) (myeval (caddr E) env)))
            ((equal? t '>) (> (myeval (cadr E) env) (myeval (caddr E) env)))

            ; IFS
            ((equal? t 'if)
             (if (myeval (cadr E) env);predicate
               (myeval (caddr E) env) ;true-action
               (myeval (cadddr E) env))) ;false-action

            ; PAIR OPERATIONS
            ((equal? t 'cons) (cons (myeval (cadr E) env) (myeval (caddr E) env)))
            ;TODO: throw error when (pair? myeval E) = #f
            ((equal? t 'car) (car (myeval (cadr E) env)) )
            ((equal? t 'cdr) (cdr (myeval (cadr E) env)) )

            ; let FIXME: use map to process list into env?
            ((equal? t 'let) 
             (let ()
               (define ht (make-hash))
               (hash-set! ht (caaadr E) (myeval (car (cdaadr E)) env))
               #;(define new-exp (map 
                                 (lambda (x) 
                                   (if (equal? x (caaadr E)) 
                                     (hash-ref ht x)
                                     x )) 
                                 (caddr E))) ;change occurrence of variable with appropriate value?
               (myeval (caddr E) (cons ht env)))) ;use mutable pair

            ((equal? t 'letrec) 
             (let ()
               (define ht (make-hash))
               (hash-set! ht (caaadr E) 'UNDEF)
               (hash-set! ht (caaadr E) (myeval (car (cdaadr E)) (cons ht env)))
               #;(define new-exp (map 
                                 (lambda (x)
                                   (if (equal? x (caaadr E)) 
                                     (hash-ref ht x)
                                     x )) 
                                 (caddr E)))
               (myeval (caddr E) (cons ht env)))) ;use mutable pair

            ((equal? t 'lambda) ;TODO: ERROR?
             (list 'lambda (cadr E) (caddr E)))

            ((list? t)
             ;(write"HERE")(newline)
             (let ()
               (define ht (make-hash))
               (hash-set! ht (caadr t) (myeval (cadr E) env))
               (myeval (caddr t) (cons ht env))))

            ;TEST
            ((equal? (car (look-up t env)) 'lambda)
             (myeval (list (look-up t env) (cadr E)) env))

            ;APPLICATION
            ;((or (equal? (car (look-up t env)) 'lambda) (list? t))



             ))))))
(define (look-up v env)
  (if (null? env) 
    (let ()
      (write "err")
      ERROR ;no such variable -> throw error
      )
    (if (not (equal? (hash-ref (car env) v 'nil) 'nil))
      (hash-ref (car env) v 'nil)
      (look-up v (cdr env)))))

;(define t '(let ((f (lambda (x) (if (= x 0) 1 2)))) (f 3)))
(define t10  '(let ((p (cons 1 (cons 2 '())))) (cons p 0)))
(myeval t10 '())
;(caddr t10)
;(define g '((lambda (x) (if (= x 0) 1 2)) 3))
;(caddr t)
;(myeval g '())

;(define t13 '(letrec ((f (lambda (x) (if (= x 0) 1 (* (f (- x 1)) x) )))) (f 5)))
;(define t13 '(letrec ((g (lambda (x) (if (= x 0) 1 (* (g (- x 1)) x) )))) (g 5)))
;(myeval t13 '())
