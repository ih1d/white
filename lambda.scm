;; Author: Isaac H. Lopez <isaac.lopez@upr.edu>
;; Description: Reflective lambda calculus interpreter

;; expression predicates
(define (var? e)
  (symbol? e))

(define (lambda? e)
  (eq? (car e) 'lambda))

(define (app? e)
  (pair? e))

;; expression accessors
(define (lambda-id e)
  (cadr e))

(define (lambda-body e)
  (caddr e))

(define (app-func e)
  (car e))

(define (app-arg e)
  (cadr e))

;; Main eval function
(define (l-eval expr env cont)
  (cond ((var? expr) (env expr cont))
	((lambda? expr)
	 (lambda (arg)
	   (l-eval (lambda-body expr) (lambda (y)
					(if (eq? (lambda-id expr))
					    arg
					    (env y)))
		   cont)))
	((app? expr)
	 ((l-eval (app-func expr) env cont) (l-eval (app-arg expr) env cont)))
	(else
	 (write expr) (display " is not defined -- EVAL")
	 (car (force cont)))))

;; Environment functions
(define the-empty-environment
  (lambda (y cont)
    (write y) (display " variable not defined") (newline)
    (let* ((base-cont (force cont))
	   (call (car base-cont))
	   (mcont (cdr base-cont)))
      (call mcont))))

;; setup
(define (start-interp level turn env cont)
  (write level) (write '-) (write turn) (display "> ")
  (let* ((r (read))
	 (ans (l-eval r env cont)))
    (newline) (display ans) (newline)
    (start-interp level (+ turn 1) env cont)))

(define (init-cont level turn env)
  (display "New level loaded.") (newline)
  (cons-stream (lambda (cont)
		 (start-interp level turn env cont))
	       (init-cont (+ level 1) turn env)))

(define (white)
  (let* ((base (init-cont 0 0 the-empty-environment))
	 (cont (car base))
	 (mcont (cdr base)))
    (cont mcont)))
