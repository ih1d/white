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

(define (var? e)
  (eq? (car e) 'var))

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

;; eval
(define (meta-eval expr env cont)
  (cond ((constant? expr) expr)
	((var? expr) (lookup expr env))
	((let? expr) (eval-let expr env))
	((if? expr) (eval-if expr env))       
	((lambda? expr) (eval-lambda expr env))
	((app? expr) (meta-apply expr env))))

(define (eval-if expr env)
  (if (meta-eval (if-pred expr) env)
      (meta-eval (if-then expr) env)
      (meta-eval (if-else expr) env)))

(define (eval-let expr env)
  (let ((new-env (map (lambda (pair)
			(extend-environment (first pair) (second pair) env))
		      (let-var&vals expr))))
    (meta-eval (let-body expr) new-env)))

(define (eval-lambda expr env)
  (lambda (arg) (meta-eval (lambda-body expr) (extend-environment (lambda-id expr) arg env))))

;; environment
(define the-empty-environment '())

(define (lookup var env)
  (cond ((null? env) (error var "unbound variable -- LOOKUP"))
	((eq? var (car (car env)))
	 (cdr (car env)))
	(else (lookup var (cdr env)))))

(define (extend-environment var val env)
  (if (null? env)
      (cons var val)
      (cons (cons var val) env)))

;; setup
(define (init-cont env level turn cont)
  (cont
   (lambda (answer)
     (write level) (write '-) (write turn) (display ": ") (write answer)
     (newline)
     (write level) (write '-) (write (+ turn 1)) (display "> ")
     (meta-eval (read) env
		(lambda (ans)
		  (init-cont env level (+ turn 1) (cont ans)))))))

(define (run env level answer)
  (init-cont env level 0
	      (lambda (cont) (cont answer))))

(define (meta-init level prev-env new-env)
  (display "New level loaded.") (newline)
  (lambda (result)
    (run (list prev-env new-env) level result)))

(define (init-meta-cont level prev-env)
  (cons-stream (meta-init level prev-env the-empty-environment)
	       (init-meta-cont (+ level 1) the-empty-environment)))

(define (main)
  (let* ((base (init-meta-cont 0 the-empty-environment))
	 (cont (head base))
	 (meta-cont (tail base)))
    ((cont 'boot) meta-cont)))

