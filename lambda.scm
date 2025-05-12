;; Author: Isaac H. Lopez <isaac.lopez@upr.edu>
;; Description: Reflective lambda calculus interpreter

;; Main eval function
(define (l-eval expr env)
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

;; Environment functions
(define the-empty-environment
  (lambda (y)
    (error y "variable not defined")))

(define init-env
  (lambda (y)
    (cond
     ((eq? y 'zero) 0)
     (else the-empty-environment))))

(define (start)
  (display "> ")
  (let ((r (read))
	(ans (l-eval r init-env)))
    (newline) (display ans)
    (start)))
