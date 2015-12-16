#lang racket
(provide myeval)
;env: list of hashtables
(define ERROR 'error)

;TODO: divide eval-helper in to modules
;TODO: allow multiple variables (use map)
(define (myeval E)
  (define (eval-helper E env) ;eval-helper: (Expression) List -> E
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
                     (hash-set! ht (car x) (eval-helper (cadr x) env))) (cadr E))
                 (eval-helper (caddr E) (cons ht env))))

              ((equal? t 'letrec) 
               (let ()
                 (define ht (make-hash))
                 (for-each 
                   (lambda (x) 
                     (hash-set! ht (car x) (eval-helper (cadr x) (cons ht env)))) (cadr E))
                 (eval-helper (caddr E) (cons ht env)))) ;use mutable pair

              ((equal? t 'lambda) ;TODO: ERROR
               (list (list 'lambda (cadr E) (caddr E)) env))

              ;APPLICATION
              ((and (list? t)(equal? 'lambda (car t)))
               (let ()
                 (define ht (make-hash))
                 (for-each 
                   (lambda (x y) 
                     (hash-set! ht x (eval-helper y env))) (cadar (eval-helper t env)) (cdr E))
                 (eval-helper (caddr t) (cons ht env))))

              ;APPLICATION with variable
              ((equal? (caar (look-up t env)) 'lambda)
                 (let ()
                   (define ht (make-hash))
                   (define clos (look-up t env))
                   (for-each 
                     (lambda (x y)
                       (hash-set! ht x (eval-helper y env))) (cadar clos) (cdr E))
                 (eval-helper (caddar clos) (cons ht (cadr clos)))))




              ))))))
  (eval-helper E '()))

(define (look-up v env)
  (if (null? env) 
    ERROR ;no such variable -> throw error
    (if (not (equal? (hash-ref (car env) v 'nil) 'nil))
      (hash-ref (car env) v 'nil)
      (look-up v (cdr env)))))

(define sum '(let ((f (lambda (x n) (if (= x 0) n (f (- x 1) (+ x n)))))) (f 999999 0)))

;(cdr (caddr tail-rec))

;(define t18 '((lambda (x y) (* x y)) 3 5))
;(myeval t18)


(myeval sum)
