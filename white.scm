;; Author: Isaac H. Lopez <isaac.lopez@upr.edu>
;; Description: Reflective lambda calculus interpreter

;; expression predicates
(define (var? e)
  (symbol? e))

(define (app? e)
  (pair? e))

(define (constant? e)
  (or (number? e)
      (boolean? e)))

(define (define? e)
  (eq? (car e) 'define))

;; expression accessors
(define (lambda-id e)
  (cadr e))

(define (lambda-body e)
  (caddr e))

(define (app-func e)
  (car e))

(define (app-arg e)
  (cdr e))

(define (define-id e)
  (if (list? (cadr e))
      (car (cadr e))
      (cadr e)))

(define (define-params e)
  (if (list? (cadr e))
      (cdr (cadr e))
      '()))

(define (define-body e)
  (caddr e))

;; Main eval function
(define (m-eval expr env)
  (cond ((constant? expr) expr)
	((if? expr) (eval-if expr))
	((let? expr) (eval-let expr))
	((lambda? expr) (eval-lambda expr env))
	((app? expr) (m-apply expr env))
	((var? expr) (lookup expr env))))
  

;; Environment functions
(define the-empty-environment '())

(define primary-environment
  (list (cons '+ +)
	(cons '- -)
	(cons '* *)
	(cons '/ /)))

;; setup
(define (white)
  (display "> ")
  (let ((r (read)))
    (display (m-eval r primary-environment))
    (white)))
