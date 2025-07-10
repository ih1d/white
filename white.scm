;; Author: Isaac H. Lopez <isaac.lopez@upr.edu>
;; Description: Reflective Language

;; eval 
(define (meta-eval expr env)
  (cond ((self-eval? expr) expr)
	((var? expr) (lookup-variable-value expr env))
	((quoted? expr) (text-of-quotation expr))
	((assignment? expr) (eval-assignment expr env))
	((definition? expr) (eval-definition expr env))
	((if? expr) (eval-if expr env))
	((lambda? expr)
	 (make-procedure (lambda-parameters expr)
			 (lambda-body expr)
			 env))
	((begin? expr)
	 (eval-sequence (begin-actions expr) env))
	((cond? expr) (meta-eval (cond->if expr) env))
	((app? expr)
	 (meta-apply (meta-eval (operator expr) env)
		     (list-of-values (operands expr) env)))
	(else (error expr "Unknown expression type -- EVAL" expr))))

;; apply
(define (meta-apply proc args)
  (cond ((primitive-procedure? proc)
	 (apply-primitive-procedure proc args))
	((compound-procedure? proc)
	 (eval-sequence
	  (procedure-body proc)
	  (extend-environment
	   (procedure-parameters proc)
	   args
	   (procedure-environment proc))))
	(else (error proc "Unknown procedure type -- APPLY"))))

;; procedure arguments
(define (list-of-values exprs env)
  (if (no-operands? exprs)
      '()
      (cons (meta-eval (first-operand exprs) env)
	    (list-of-values (rest-operands exprs) env))))

;; conditionals
(define (eval-if expr env)
  (if (true? (meta-eval (if-pred expr) env))
      (meta-eval (if-then expr) env)
      (meta-eval (if-else expr) env)))

;; sequences
(define (eval-sequence exprs env)
  (cond ((last-expr? exprs) (meta-eval (first-expr exprs) env))
	(else (meta-eval (first-expr exprs) env)
	      (eval-sequence (rest-exprs exprs) env))))

;; assignment & definition
(define (eval-assigment expr env)
  (set-variable-value! (assignment-variable expr)
		       (meta-eval (assigment-value expr) env)
		       env))

(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
    (meta-eval (definition-value expr) env)
    env)
  'ok)

;; expressions
(define (self-eval? expr)
  (cond ((number? expr) true)
	((string? expr) true)
	(else false)))

(define (var? expr) (symbol? expr))

(define (quoted? expr)
  (tagged-list? expr 'quote))

(define (text-of-quotation expr) (cadr expr))

(define (tagged-list? expr tag)
  (if (pair? expr)
      (eq? (car expr) tag)
      false))

(define (assignment? expr)
  (tagged-list? expr 'set!))

(define (assignment-variable expr) (cadr expr))

(define (assignment-value expr) (caddr expr))

(define (definition? expr)
  (tagged-list? expr 'define))

(define (definition-variable expr)
  (if (symbol? (cadr expr))
      (cadr expr)
      (caadr expr)))

(define (definition-value expr)
  (if (symbol? (cadr expr))
      (caddr expr)
      (make-lambda (cdadr expr)
		   (cddr expr))))

(define (lambda? expr) (tagged-list? expr 'lambda))

(define (lambda-parameters expr) (cadr expr))

(define (lambda-body expr) (cddr expr))

(define (make-lambda params body)
  (cons 'lambda (cons params body)))

(define (if? expr) (tagged-list? expr 'if))

(define (if-pred expr) (cadr expr))

(define (if-then expr) (caddr expr))

(define (if-else expr)
  (if (not (null? (cdddr expr)))
      (cadddr expr)
      'false))

(define (make-if pred then alt)
  (list 'if pred then alt))

(define (begin? expr) (tagged-list? expr 'begin))

(define (begin-actions expr) (cdr expr))

(define (last-expr? seq) (null? (cdr seq)))

(define (first-expr seq) (car seq))

(define (rest-exprs seq) (cdr seq))

(define (sequence->expr seq)
  (cond ((null? seq) seq)
	((last-expr? seq) (first-expr seq))
	(else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (app? expr) (pair? expr))

(define (operator expr) (car expr))

(define (operands expr) (cdr expr))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;; derived expressions
(define (cond? expr) (tagged-list? expr 'cond))

(define (cond-clauses expr) (cdr expr))

(define (cond-else-clause? clause)
  (eq? (cond-pred clause) 'else))

(define (cond-pred clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if expr)
  (expand-clauses (cond-clauses expr)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((fst (car clauses))
	    (rst (cdr clauses)))
	(if (cond-else-clause? fst)
	    (if (null? rst)
		(sequence->expr (cond-actions fst))
		(error clauses "ELSE clause isn't last -- COND->IF"))
	    (make-if (cond-pred fst)
		     (sequence->expr (cond-actions fst))
		     (expand-clauses rst))))))

;; predicates
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; Representing procedures
(define (make-procedure params body env)
  (list 'procedure params body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;; Environment
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error vars "Too many arguments supplied")
	  (error vars "Too few arguments supplied"))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error var "Unbound variable")
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedur? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;; setup
(define (init-cont env level turn answer)
  (lambda (meta-cont)    
    (write level) (write '-) (write turn) (display ": ") (write answer)
    (newline)
    (write level) (write '-) (write (+ turn 1)) (display "> ")
    (meta-eval (read) env
	       (lambda (ans)
		 (init-cont env level (+ turn 1) ans)))))

(define (run env level)
  (init-cont env level 0 'boot))

(define (meta-init level prev-env new-env)
    (display "New level loaded.") (newline)
    (run (list prev-env new-env) level))

(define (init-meta-cont level prev-env)
  (cons-stream (meta-init level prev-env the-empty-environment)
	       (init-meta-cont (+ level 1) the-empty-environment)))

;; loop
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (meta-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     '<procedure-env>))
      (display object)))

(define (main)
  (let* ((base (init-meta-cont 0 the-empty-environment))
	 (cont (head base))
	 (meta-cont (tail base)))
    ((cont) meta-cont)))
