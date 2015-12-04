#lang racket
(provide myeval)
;env: list of hashtables
(define ERROR 'error)

;TODO: divide eval-helper in to modules
;TODO: allow multiple variables (use map)
(define (myeval E)
  (define (eval-helper E env) ;eval-helper: (Expression) List -> E
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
              ((equal? t '+) (+ (eval-helper (cadr E) env) (eval-helper (caddr E) env))) 
              ((equal? t '-) (- (eval-helper (cadr E) env) (eval-helper (caddr E) env)))
              ((equal? t '*) (* (eval-helper (cadr E) env) (eval-helper (caddr E) env)))
              ((equal? t '=) (= (eval-helper (cadr E) env) (eval-helper (caddr E) env)))
              ((equal? t '<) (< (eval-helper (cadr E) env) (eval-helper (caddr E) env)))
              ((equal? t '>) (> (eval-helper (cadr E) env) (eval-helper (caddr E) env)))

              ; IFS
              ((equal? t 'if)
               (if (eval-helper (cadr E) env);predicate
                 (eval-helper (caddr E) env) ;true-action
                 (eval-helper (cadddr E) env))) ;false-action

              ; PAIR OPERATIONS
              ((equal? t 'cons) (cons (eval-helper (cadr E) env) (eval-helper (caddr E) env)))
              ;TODO: throw error when (pair? eval-helper E) = #f
              ((equal? t 'car) (car (eval-helper (cadr E) env)) )
              ((equal? t 'cdr) (cdr (eval-helper (cadr E) env)) )

              ; let FIXME: use map to process list into env?
              ((equal? t 'let) 
               (let ()
                 (define ht (make-hash))
                 (for-each 
                   (lambda (x) 
                     (hash-set! ht (car x) (eval-helper (cadr x) env))) (cadr E)) ;TODO:ask question
                 ;change occurrence of variable with appropriate value?
                 (eval-helper (caddr E) (cons ht env)))) ;use mutable pair

              ((equal? t 'letrec) 
               (let ()
                 (define ht (make-hash))
                 (for-each 
                   (lambda (x) 
                     (hash-set! ht (car x) 'UNDEF)) (cadr E)) ;TODO:ask question
                 (for-each 
                   (lambda (x) 
                     (hash-set! ht (car x) (eval-helper (cadr x) (cons ht env)))) (cadr E))
                 (eval-helper (caddr E) (cons ht env)))) ;use mutable pair

              ((equal? t 'lambda) ;TODO: ERROR?
               (list 'lambda (cadr E) (caddr E)))

              ;APPLICATION
              ((list? t)
               (let ()
                 (define ht (make-hash))
                 (for-each 
                   (lambda (x y) 
                     (hash-set! ht x (eval-helper y env))) (cadr t) (cdr E))
                 (eval-helper (caddr t) (cons ht env))))

              ((equal? (car (look-up t env)) 'lambda)
               (eval-helper (cons (look-up t env) (cdr E)) env)) ;FIXME for multi-variables

              ;((or (equal? (car (look-up t env)) 'lambda) (list? t))



              ))))))
  (eval-helper E '()))

(define (look-up v env)
  (if (null? env) 
    ERROR ;no such variable -> throw error
    (if (not (equal? (hash-ref (car env) v 'nil) 'nil))
      (hash-ref (car env) v 'nil)
      (look-up v (cdr env)))))

;(define tail-rec '(letrec ((f (lambda (x n) (if (= x 0) n (f (- x 1) (+ n x)) )))) (f 100 0)))

;(cdr (caddr tail-rec))

;(define t18 '((lambda (x y) (+ x y)) 3 5))
;(cdr t18)


(define tail-rec '(letrec ((f (lambda (x n) (if (= x 0) n (f (- x 1) (+ n x)) )))) (f 5 0)))
(myeval tail-rec)
