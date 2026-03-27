;; Author: Isaac H. Lopez Diaz
;; Description: Main module
;; Licensed under GNU GPLv3
(load "env.scm")

;; expressions
(define (constant? e)
  (or (number? e)
      (boolean? e)))

;; BLUE ;;
(define (blue-eval expr env str)
  (cond ((constant? expr) expr)
	(else (error expr "not defined yet!"))))


(define (blue-repl level turn env str)
  (lambda (answer)
    (lambda (Mcont)
      (write level) (write '-) (write turn) (display " :: ") (write answer)
      (newline)
      (write level) (write '-) (write (+ turn 1)) (display " BLUE> ")
      (let ((r (read)))
	(((blue-repl level (+ turn 1) env str)
	  (blue-eval r env str))
	 Mcont)))))

;; GRAPH ;;
(define (graph-eval expr env str)
  (cond ((constant? expr) expr)
	(else (error expr "not defined yet!"))))

(define (graph-repl level turn env str)
  (lambda (answer)
    (lambda (Mcont)
      (write level) (write '-) (write turn) (display " :: ") (write answer)
      (newline)
      (write level) (write '-) (write (+ turn 1)) (display " GRAPH> ")
      (let ((r (read)))
	(((graph-repl level (+ turn 1) env str)
	  (graph-eval r env str))
	 Mcont)))))

;; initialization
(define (init env str)
  (cons (blue-repl 0 0 env str)
	(graph-repl 0 0 env str)))

(define (main)
  (let* ((repls (init '() '()))
	 (blue (car repls))
	 (graph (cdr repls)))
    ((blue 'start) graph)))
