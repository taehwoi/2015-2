#lang racket
(provide myeval)
;env: list of hashtables
(define ERROR 'error)

;TODO: divide myeval in to modules
(define (myeval E env) ;myeval: (Expression) List -> E
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
               (myeval (caddr exp) (cons ht env))))

            ((equal? t 'letrec) 
             (let ()
               (define ht (make-hash))
               (hash-set! ht (caaadr E) (myeval (car (cdaadr E)) (cons ht env)))
               (myeval (caddr exp) (cons ht env))))

            ((equal? t 'lambda) ;TODO: ERROR?
             (list 'fun (cadr E) `-> (caddr E)))

            ;APPLICATION
            ((list? t 'FIXME))

            ))))))
(define (look-up v env)
  (if (null? env) 
    ERROR ;no such variable -> throw error
    (if (not (equal? (hash-ref (car env) v 'nil) 'nil))
      (hash-ref (car env) v 'nil)
      (look-up v (cdr env)))))

(define exp  '(letrec ((p (cons 1 (cons 2 '())))) (cons 0 p)))
(myeval exp '())
