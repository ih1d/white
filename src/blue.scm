;; Author: Isaac H. Lopez Diaz
;; Description: Main module
;; Licensed under GNU GPLv3
(load "env.scm")

;; expressions
(define (constant? e)
  (or (number? e)
      (boolean? e)))

(define (if? e)
  (eq? 'if (car e)))

;; BLUE ;;
(define (blue-eval expr env str)
  (cond ((constant? expr) expr)
	((if? expr)
	 (blue-eval-if expr env str))
	(else (error expr "not defined yet!"))))

(define (blue-eval-if expr env str)
  (let* ((cnd (cadr expr))
	 (thn (caddr expr))
	 (els (if (null? (cdddr expr))
		  #f
		  (cadddr expr))))
    (if (blue-eval cnd env str)
	(blue-eval thn env str)
	(blue-eval els env str))))

(define (blue-repl level turn env str)
  (lambda (answer)
    (write level) (write '-) (write turn) (display " :: ") (write answer)
    (newline)
    (write level) (write '-) (write (+ turn 1)) (display " BLUE> ")
    ((blue-repl level (+ turn 1) env str)
     (blue-eval (read) env str))))

(define (blue)
  ((blue-repl 0 0 '() '()) 'start))
