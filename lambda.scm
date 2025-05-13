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
  (car (cadr e)))

(define (lambda-body e)
  (caddr e))

(define (app-func e)
  (car e))

(define (app-arg e)
  (cadr e))

;; Main eval function
(define (l-eval expr env cont)
  (cond ((number? expr) expr)
	((var? expr) (env expr cont))
	((lambda? expr)
	 (lambda (arg)
	   (l-eval (lambda-body expr) (lambda (y mcont)
					(if (eq? y (lambda-id expr))
					    arg
					    (env y mcont)))
		   cont)))
	((app? expr)
	 ((l-eval (app-func expr) env cont)
	  (l-eval (app-arg expr) env cont)))))

;; Environment functions
(define (add1 x) (+ x 1))

(define (zero? x) (= x 0))

(define the-environment
  (lambda (y cont)
    (cond ((eq? y 'add1) add1)
	  ((eq? y 'zero?) zero?)
	  (else
	   (write y) (display " variable not defined") (newline)
	   (let* ((base-cont (force cont))
		  (call (car base-cont))
		  (mcont (cdr base-cont)))
	     (call mcont))))))

;; setup
(define (start-interp level turn env cont)
  (write level) (write '-) (write turn) (display "> ")
  (let* ((r (read))
	 (ans (l-eval r env cont)))
    (display ans) (newline)
    (start-interp level (+ turn 1) env cont)))

(define (init-cont level turn env)
  (display "New level loaded.") (newline)
  (cons-stream (lambda (cont)
		 (start-interp level turn env cont))
	       (init-cont (+ level 1) turn env)))

(define (white)
  (let* ((base (init-cont 0 0 the-environment))
	 (cont (car base))
	 (mcont (cdr base)))
    (cont mcont)))
