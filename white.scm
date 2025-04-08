;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: White language for reflective type checkers
;; Licensed under MIT

;; PROCEDURES FOR ENV ;;
(define the-empty-env '())

;; USER LEVEL 0 -> SIMPLY TYPED LAMBDA CALCULUS
;; META LEVEL 1 -> SYSTEM F/SYSTEM w (polymorphism)
;; META LEVEL 2 -> DEPENDENT TYPES
;; META LEVEL 3 -> HOMOTOPY TYPES

;; Expressions are:
;; Numbers
;; Booleans
;; Variables
;; Lambda
;; Binary Operations (+ - and or)
(define-structure expression type expr)

(define parser
  (lambda (inp)
    (if (list? inp)
	(cond
	 ((null? inp) '())
	 ((number? (car inp))
	  (cons (make-expression 'Number (car inp))
		(parser (cdr inp))))
	 ((boolean? (car inp))
	  (cons (make-expression 'Boolean (car inp))
		(parser (cdr inp))))
	 ((eq? 'lambda (car inp))
	  (let ((types (cadr inp))
		(vars (caddr inp)))
	    (if (list? types)
		'()
		(cons (make-expression 'Type types)
		      (
	 ((symbol? (car inp))
	  (cons (make-expression 'Var (car inp))
		(parser (cdr inp))))
	 (else (error (car inp) "not defined")))
	(cond
	 ((number? inp)
	  (make-expression 'Number inp))
	 ((boolean? inp)
	  (make-expression 'Boolean inp))
	 ((symbol? inp)
	  (make-expression 'Var inp))
	 (else (error inp "not defined")))))))

;; check if input is operator
(define (is-operator op)
  (cond ((eq? op '+) #t)
	((eq? op '-) #t)
	((eq? op 'and) #t)
	(else op "not defined")))
	
;; type checker takes an expression and returns a type
(define (tc e)
  (cond
   ((eq? 'Number (expression-type e))
    'Number)
   ((eq? 'Boolean (expression-type e))
    'Boolean)
   ((eq? 'BinOp (expression-type e))
    (bin-decision (car (expression-expr e))
		  (tc (cadr (expression-expr e)))
		  (tc (caddr (expression-expr e)))))  
   (else error e "not defined")))

;; binary decision
(define (bin-decision op t1 t2)
  (cond
   ((eq? op '+)
    (if (and (eq? 'Number t1)
	     (eq? 'Number t2))
	'Number
	(error (list t1 t2) "type mismatch")))
   ((eq? op '-)
    (if (and (eq? 'Number t1)
	     (eq? 'Number t2))
	'Number
	(error (list t1 t2) "type mismatch")))
   ((eq? op 'and)
    (if (and (eq? 'Boolean t1)
	     (eq? 'Boolean t2))
	'Boolean
	(error (list t1 t2) "type mismatch")))
   (else
    (error op "not defined"))))
