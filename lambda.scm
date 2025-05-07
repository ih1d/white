;; Author: Isaac H. Lopez <isaac.lopez@upr.edu>
;; Description: Reflective lambda calculus interpreter

(define (l-eval expr env cont)
  (if (list? expr)
      (cond
       ((single? expr)
	(l-eval (car expr) env cont))
       ((eq? 'lambda (car expr))
	(lambda (arg)
	  (l-eval (third expr) (lambda (y)
			       (if (eq? y (second expr))
				   arg
				   (env y)))
		  cont)))
       ((pair? expr)
	((l-eval (car expr) env cont) (l-eval (cdr expr) env cont)))
       (else (error (car expr) "not a valid expression -- EVAL")))
      (cond
       ((number? expr) expr)
       ((symbol? expr) (env expr))
       (else (error expr "not a valid expression -- EVAL")))))

(define (single? ls)
  (= (length ls) 1))

(define add1
  (lambda (x) (+ x 1)))

(define the-empty-environment
  (lambda (y)
    (error y "variable not defined")))

(define init-env
  (lambda (y)
    (cond ((eq? y 'add1) add1)
	  ((eq? y 'zero) 0)
	  (else the-empty-environment))))

(define (l-init level turn env)
  (write level) (write '-) (write turn) (display "> ")
  (let* ((r (read))
	 (ans (l-eval r env
		      (lambda (cont)
			(l-init level (+ turn 1) env)))))
    (display ans) (newline)
    (l-init level (+ turn 1) env)))

(define (l-meta level turn env)
  (cons-stream (l-init level turn env)
	       (l-meta (+ 1 level) turn env)))

(define (start)
  (let* ((base-l-cont (l-meta 0 0 init-env))
	 (cont (car base-l-cont))
	 (mcont (cdr base-l-cont)))
    ((cont 'start) mcont)))
