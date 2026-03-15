;; Author: Isaac H. Lopez Diaz
;; Description: Main module
(load "tensor.scm")

;; predicates
(define (constant? expr)
  (or (number? expr)
      (boolean? expr)))

(define (if? expr)
  (eq? 'if (car expr)))

(define (switch? expr)
  (eq? 'switch (car expr)))

;; accessors
(define (if-pred expr) (cadr expr))

(define (if-then expr) (caddr expr))

(define (if-else expr)
  (let ((melse (cdddr expr)))
    (if (null? melse)
	#f
	(cadddr expr))))

;; eval
(define (blue-base-eval expr env cont str)
  (cond ((constant? expr) (cont expr))
	((if? expr) (eval-if expr env cont str))
	(else (error expr "unknown construct"))))

(define (eval-if expr env cont str)
  (blue-base-eval (if-pred expr)
		  env
		  (lambda (p)
		    (if p
			(blue-base-eval (if-then expr) env cont str)
			(blue-base-eval (if-else expr) env cont str)))
		  str))

;; repl
(define (blue env str level turn)
  (lambda (ans)
    (lambda (Mcont)
      (write level) (write '-) (write turn) (display ": ")
      (write ans)
      (newline)
      (write level) (write '-) (write (+ 1 turn)) (display " BLUE ]=> ")
      (blue-base-eval (read) env (lambda (ans)
				   (((blue env str level (+ turn 1)) ans) Mcont))
		      str))))

(define (graph level turn)
  (lambda (ans)
    (lambda (Mcont)
      (write level) (write '-) (write turn) (display ": ")
      (write ans)
      (newline)
      (write level) (write '-) (write (+ 1 turn)) (display " GRAPH ]=> ")
      (let ((r (read)))
	(if (eq? r 'switch)
	    ((Mcont '(switch to blue)) (graph level turn))
	    (((graph level (+ turn 1)) r) Mcont))))))

;; initialize
(define (init-repls)
  (cons (blue '() '() 0 0)
	(graph 0 0)))

(define (main)
  (let* ((repls (init-repls))
	 (blue-repl (car repls))
	 (graph-repl (cdr repls)))
    ((blue-repl 'start) graph-repl)))
