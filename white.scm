;; Author: Isaac H. Lopez <isaac.lopez@upr.edu>
;; Description: Reflective Language

;; expression
(define (constant? e)
  (or (number? e)
      (boolean? e)))

(define (if? e)
  (eq? (car e) 'if))

(define (let? e)
  (eq? (car e) 'let))

(define (error? e)
  (eq? e 'error))

(define (var? e)
  (symbol? e))

(define (lambda? e)
  (eq? (car e) 'lambda))

(define (app? e)
  (pair? e))

;; accessors
(define (if-pred e)
  (cadr e))

(define (if-then e)
  (caddr e))

(define (if-else e)
  (cadddr e))

(define (let-var&vals e)
  (define (loop pairs)
    (cond ((null? pairs) '())
	  (else (cons (car pairs)
		      (loop (cdr pairs))))))
  (loop (cadr e)))

(define (let-body e)
  (caddr e))

(define (lambda-id e)
  (car (cadr e)))

(define (lambda-body e)
  (caddr e))

;; eval (the tricky bit)
(define (meta-eval expr env cont)
  (cond ((error? expr)
	 (lambda ()
	   (lambda (meta-cont)
	     (let* ((new-cont (head meta-cont))
		    (new-meta-cont (tail meta-cont)))
	       ((new-cont) new-meta-cont)))))
	((constant? expr) (cont expr))
	((var? expr)
	 (let ((val (lookup expr (car env))))
	   (meta-eval val env cont)))
	((let? expr) (eval-let expr env cont))
	((if? expr) (eval-if expr env cont))       
	((lambda? expr) (meta-eval 'error env cont)) ;; no se como hacer (eval-lambda expr env cont))
	((app? expr) (meta-eval 'error env cont)))) ;; tampoco (eval-apply expr env cont))))

(define (eval-if expr env cont)
  (meta-eval (if-pred expr) env
	     (lambda (ans)
	       (if ans
		   (meta-eval (if-then expr) env cont)
		   (meta-eval (if-else expr) env cont)))))

(define (eval-let expr env cont)
  (define (loop pairs curr-env)
    (cond ((null? pairs) curr-env)
	  (else
	   (let* ((pair (car pairs))
		  (var (first pair))
		  (val (second pair)))
	     (loop (cdr pairs) (extend-environment var val curr-env))))))
  (let* ((new-curr-env (loop (let-var&vals expr) (car env)))
	 (new-env (cons new-curr-env (cdr env))))
    (meta-eval (let-body expr) new-env cont)))

;; environment
(define the-empty-environment '())

(define (extend-environment var val env)
  (cons (cons var val) env))

(define (lookup var env)
  (cond ((null? env)
	 (write var) (display " unbound variable.") (newline)
	 'error)
	((eq? var (car (car env)))
	 (cdr (car env)))
	(else (lookup var (cdr env)))))

;; setup
(define (init-cont env level turn answer)
  (write level) (write '-) (write turn) (display ": ") (write answer)
  (newline)
  (write level) (write '-) (write (+ turn 1)) (display "> ")
  (meta-eval (read) env
	     (lambda (ans)
	       (init-cont env level (+ turn 1) ans))))

(define (run env level)
  (init-cont env level 0 'boot))

(define (meta-init level prev-env new-env)
    (display "New level loaded.") (newline)
    (run (list prev-env new-env) level))

(define (init-meta-cont level prev-env)
  (cons-stream (meta-init level prev-env the-empty-environment)
	       (init-meta-cont (+ level 1) the-empty-environment)))

(define (main)
  (let* ((base (init-meta-cont 0 the-empty-environment))
	 (cont (head base))
	 (meta-cont (tail base)))
    ((cont) meta-cont)))
