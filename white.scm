;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: White language for reflective type checkers
;; Licensed under MIT

;; USER LEVEL 0 -> SIMPLY TYPED LAMBDA CALCULUS
;; META LEVEL 1 -> SYSTEM F/SYSTEM w (polymorphism)
;; META LEVEL 2 -> DEPENDENT TYPES
;; META LEVEL 3 -> HOMOTOPY TYPES

;; environment functions
(define the-empty-environment '())

;; type checker takes an expression and returns a type
(define (tc exp env))

;; is operator
(define (op? e)
  (cond
   ((eq? e '+) #t)
   ((eq? e '*) #t)
   ((eq? e 'and) #t)
   ((eq? e 'or) #t)))

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
