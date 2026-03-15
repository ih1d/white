;; Author: Isaac H. Lopez Diaz
;; Description: Environment functions and ds

(define the-empty-environment '())

(define initial-environment
  (list (cons 'car car)
	(cons 'cdr cdr)
	(cons 'cons cons)
	(cons 'null? null?)
	(cons '+ +)
	(cons '- -)
	(cons '* *)
	(cons '/ /)
	(cons '> >)
	(cons '< <)
	(cons '= =)))

(define (lookup op env)
  (cond ((null? env) '())
	((eq? op (car (car env)))
	 (cdr (car env)))
	(else (lookup op (cdr env)))))

(define (insert proc body env)
  (cons (cons proc body) env))
