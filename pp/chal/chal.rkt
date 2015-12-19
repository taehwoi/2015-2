#lang racket
(provide myeval)

;env: list of hashtables

(define (myeval E)

  (define (eval-helper E env) ;eval-helper: (Expression) -> E
    (if (equal? E ''()) '() ;Null
      (if (or (number? E) (boolean? E)) E ;Value
        (if (not (list? E)) ;start evaluation
          (let ()
            (look-up E env)) ;Variable
          (let () ;composite expressions
            (define type (car E))
            (cond 
              ; Binary operation
              ((binary? type) (binary-eval type E env))
              ; Pair operation
              ((pair-op? type) (pair-eval type E env))
              ; IF (b, t, f) operation
              ((if-op? type) (if-eval type E env))
              ; LET, LETREC
              ((assign-op? type) (assign-eval type E env))
              ; LAMBDA
              ((equal? type 'lambda)
               (list (list 'lambda (cadr E) (caddr E)) env))
              ;APPLICATION
              ((app-op? type E env) (app-eval type E env))
              (else (raise "unimplmented keyword"))

              ))))))

  (define (binary? op)
    (cond
      ((equal? op '+) #t) 
      ((equal? op '-) #t)
      ((equal? op '*) #t)
      ((equal? op '=) #t)
      ((equal? op '<) #t)
      ((equal? op '>) #t)
      ((equal? op 'cons) #t)
      (else #f)))

  (define (binary-eval op E env)
    (define a (eval-helper (cadr E) env))
    (define b (eval-helper (caddr E) env))
    (if (equal? op 'cons)
      (cons a b) ;no need for the values to be int
      (let ()
        (cond
          ((or (not (number? a)) (not (number? b))) 
           (raise (~a "Error: " op " expects Int, given: " a ", " b)))
          ((equal? op '+) (+ a b)) 
          ((equal? op '-) (- a b))
          ((equal? op '*) (* a b))
          ((equal? op '=) (= a b))
          ((equal? op '<) (< a b))
          ((equal? op '>) (> a b))
          (else (raise "undefined binary procedure"))))))

  (define (pair-op? op)
    (cond
      ((equal? op 'car) #t) 
      ((equal? op 'cdr) #t)
      (else #f)))

  (define (pair-eval op E env)
    (define p (eval-helper (cadr E) env))
    (if (pair? p)
      (cond
        ((equal? op 'car) (car p))
        ((equal? op 'cdr) (cdr p))
        (else (raise "undefined pair operation")))
      (raise (~a "Error: " op " expects a Pair, given: " p))))

  (define (if-op? op)
    (cond
      ((equal? op 'if) #t) 
      (else #f)))

  (define (if-eval type E env)
    (if (eval-helper (cadr E) env) ;condition
      (eval-helper (caddr E) env) ;true-action
      (eval-helper (cadddr E) env))) ;false-action 

  (define (assign-op? op)
    (cond
      ((equal? op 'let) #t) 
      ((equal? op 'letrec) #t) 
      (else #f)))

  (define (assign-eval op E env)
    (define ht (make-hash))
    (let ()
      (cond  
        ((equal? op 'let)
         (for-each
           (lambda (x) 
             (if (hash-has-key? ht (car x)) 
               (raise "duplicate definition")
             (hash-set! ht (car x) (eval-helper (cadr x) env))))
           (cadr E)))
        ((equal? op 'letrec)
         (for-each
           (lambda (x) 
             (if (hash-has-key? ht (car x)) 
               (raise "duplicate definition")
             (hash-set! ht (car x) 'UNDEF))) (cadr E))
         (for-each
           (lambda (x) 
             (hash-set! ht (car x) (eval-helper (cadr x) (cons ht env)))) 
           (cadr E))))
         (eval-helper (caddr E) (cons ht env))))

  (define (app-op? op E env)
    (cond
      ((not (list? (eval-helper op env))) 
       (raise "Error: Expected a procedure"))
      ((equal? (caar (eval-helper op env)) 'lambda) #t)
      (else #f)))

   (define (app-eval op E env)
    (define ht (make-hash))
    (let ()
      (cond  
        ((equal? (caar (eval-helper op env)) 'lambda)
         (let ()
           (define proc (eval-helper op env))
           (for-each 
             (lambda (x y)
               (hash-set! ht x (eval-helper y env))) (cadar proc) (cdr E))
           (eval-helper (caddar proc) (cons ht (cadr proc)))))
        (else (raise "Error: undefined application operation")))))

  ;here comes the pokemon master
  ( with-handlers 
      ;Error raised during run-time
    ( ((lambda (x) (string? x)) (lambda (x) (raise x))) 
      ;Error raised during parsing
      ((lambda (x) #t) (lambda (x) (raise "Error: Ill-formed-syntax"))) )
    (eval-helper E '()) )
  )

(define (look-up v env)
  (if (null? env) 
    (raise (~a "Error: " v " is undefined"))
    (if (hash-has-key? (car env) v)
      (if (equal? (hash-ref (car env) v 'nil) 'UNDEF)
        (raise (~a "Error: " v " is undefined"))
        (hash-ref (car env) v 'UNDEF))
      (look-up v (cdr env)))))
