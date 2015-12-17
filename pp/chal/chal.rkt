#lang racket
(provide myeval)
;env: list of hashtables
(define ERROR 'error)

;TODO: divide eval-helper in to modules
;TODO: allow multiple variables (use map)
(define (myeval E)
  (define (bin-eval op E env)
    (define a (eval-helper (cadr E) env))
    (define b (eval-helper (caddr E) env))
    (if (equal? op cons)
      (cons a b) ;no need for the values to be int
      (let ()
        (cond
          ((or (not (number? a)) (not (number? b))) (raise "Error: Expect a Int"))
          (else (op a b))))))

  (define (eval-helper E env) ;eval-helper: (Expression) List -> E
    (if (equal? E ''()) '() ;Null
      (if (or (number? E) (boolean? E)) E ;Value
        (if (not (list? E)) ;start evaluation
          (let ()
            (look-up E env)) ;variable -> look up environment
          (let () ;composite expressions
            (define type (car E))
            (cond 
              ; Binary operation - nothing but just application????
              ((equal? type '+) (bin-eval + E env)) 
              ((equal? type '-) (bin-eval - E env))
              ((equal? type '*) (bin-eval * E env))
              ((equal? type '=) (bin-eval = E env))
              ((equal? type '<) (bin-eval < E env))
              ((equal? type '>) (bin-eval > E env))
              ((equal? type 'cons) (bin-eval cons E env))

              ((equal? type 'car) 
                 (let ()
                   (define p (eval-helper (cadr E) env))
                 (if (pair? p)
                   (car p) 
                   (raise "Error: Expect a Pair"))))

              ((equal? type 'cdr) 
                 (let ()
                   (define p (eval-helper (cadr E) env))
                 (if (pair? p)
                   (cdr p) 
                   (raise "Error: Expect a Pair"))))

              ; IFS
              ((equal? type 'if)
               (if (eval-helper (cadr E) env);predicate
                 (eval-helper (caddr E) env) ;true-action
                 (eval-helper (cadddr E) env))) ;false-action

              ((equal? type 'let) 
               (let ()
                 (define ht (make-hash))
                 (for-each
                   (lambda (x) 
                     (hash-set! ht (car x) (eval-helper (cadr x) env))) (cadr E))
                 (eval-helper (caddr E) (cons ht env))))

              ((equal? type 'letrec) 
               (let ()
                 (define ht (make-hash))
                 (for-each 
                   (lambda (x) 
                     (hash-set! ht (car x) (eval-helper (cadr x) (cons ht env)))) (cadr E))
                 (eval-helper (caddr E) (cons ht env))))

              ((equal? type 'lambda)
               (list (list 'lambda (cadr E) (caddr E)) env))

              ;APPLICATION
              ((and (list? type) (equal? 'lambda (car type)))
               (let ()
                 (define ht (make-hash))
                 (for-each 
                   (lambda (x y) 
                     (hash-set! ht x (eval-helper y env))) (cadar (eval-helper type env)) (cdr E))
                 (eval-helper (caddr type) (cons ht env))))

              (not (list? (look-up type env) )
                   (raise "Error: Expected a procedure"))
              ;APPLICATION with variable
              ((equal? (caar (look-up type env)) 'lambda)
                (write (caar (look-up type env)))
                 (let ()
                   (define ht (make-hash))
                   (define clos (look-up type env))
                   (for-each 
                     (lambda (x y)
                       (hash-set! ht x (eval-helper y env))) (cadar clos) (cdr E))
                 (eval-helper (caddar clos) (cons ht (cadr clos)))))





              ))))))

  ;here comes the pokemon master
  (with-handlers 
    ( ((lambda (x) (string? x)) (lambda (x) (raise x))) ;Error raised during run-time
      ((lambda (x) #t) (lambda (x) (raise "Error: Ill-formed-syntax"))) ) ;Error raised during parsing
    (eval-helper E '())))

(define (look-up v env)
  (if (null? env) 
    (raise (string-append "Error: " (~a v) " is undefined"))
    (if (not (equal? (hash-ref (car env) v 'nil) 'nil))
      (hash-ref (car env) v 'nil)
      (look-up v (cdr env)))))


;(cdr (caddr tail-rec))

;(define t18 '((lambda (x y) (* x y)) 3 5))
;(myeval t18)


(define t10  '(let ((x (+ 2 1))) (x 5)))
(myeval t10)
