;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Environment module

(define the-empty-environment '())

(define (lookup var env)
  (cond ((null? env) 'nothing)
	((eq? var (car (car env)))
	 (cdr (car env)))
	(else (lookup var (cdr env)))))

(define (extend-environment env var val)
  (define (env-loop env)
    (cond ((eq? var (car (car env)))
	   (set-car! (cons var val) env)
	   env)
	  (else (cons (car env) (env-loop (cdr env))))))
  (if (eq? 'nothing (lookup var env))
      (cons (cons var val) env)
      (env-loop env)))

