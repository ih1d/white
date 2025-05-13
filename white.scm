;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Main module for white reflective language
;; Licensed under GPLv3

;; constructors
(define (make-var id)
  (list 'var id))

(define (make-app func arg)
  (list 'app func arg))

(define (make-abs id body)
  (list 'abs id body))

(define (make-let id e1 e2 e3)
  (list 'let id e1 e2 e3))

(define (make-pi id e1 e2)
  (list 'pi id e1 e2))

(define (make-type)
  (list 'type))

(define (make-vgen int)
  (list 'vgen int))

(define (make-vapp func arg)
  (list 'vapp func arg))

(define (make-vtype)
  (list 'vtype))

(define (make-vclos env expr)
  (list 'vclos env expr))

;; predicates
(define (var? e)
  (eq? (car e) 'var))

(define (app? e)
  (eq? (car e) 'app))

(define (abs? e)
  (eq? (car e) 'abs))

(define (let? e)
  (eq? (car e) 'let))

(define (pi? e)
  (eq? (car e) 'pi))

(define (type? e)
  (eq? (car e) 'type))

(define (vgen? e)
  (eq? (car e) 'vgen))

(define (vapp? e)
  (eq? (car e) 'vapp))

(define (vtype? e)
  (eq? (car e) 'vtype))

(define (vclos? e)
  (eq? (car e) 'vclos))

;; accessors
(define (var-id e)
  (cadr e))

(define (app-func e)
  (cadr e))

(define (app-arg e)
  (caddr e))

(define (abs-id e)
  (cadr e))

(define (abs-body e)
  (caddr e))

(define (let-id e)
  (cadr e))

(define (let-expr1 e)
  (caddr e))

(define (let-expr2 e)
  (cadddr e))

(define (let-expr3 e)
  (fifth e))

(define (pi-id e)
  (cadr e))

(define (pi-expr1 e)
  (caddr e))

(define (pi-expr2 e)
  (cadddr e))

(define (vgen-int e)
  (cadr e))

(define (vapp-func e)
  (cadr e))

(define (vapp-arg e)
  (caddr e))

(define (vclos-env e)
  (cadr e))

(define (vclos-expr e)
  (caddr e))

;; environment functions
(define (update env id val)
  (cons (cons id val) env))

(define (lookup id env)
  (cond ((null? env) (error id "variable not found -- LOOKUP"))
	((eq? id (car (car env)))
	 (cdr (car env)))
	(else (lookup id (cdr env)))))


;; whnf algorithm
(define (app u v)
  (cond ((and (vclos? u) (abs? (vclos-expr u)))	      
	 (let* ((env (vclos-env u))
		(x (abs-id (vclos-expr u)))
		(e (abs-body (vclos-expr u))))
	   (eval (update env x v) e)))
	(else (make-app u v))))

(define (eval env e)
  (cond ((var? e) (lookup (var-id e) env))
	((app? e)
	 (let* ((e1 (app-func e))
		(e2 (app-arg e)))
	   (app (eval env e1) (eval env e2))))
	((let? e)
	 (let* ((x (let-id e))
		(e1 (let-expr1 e))
		(e3 (let-expr3 e)))
	   (eval (update env x (eval env e1)) e3)))
	((type? e) (make-vtype))
	(else (make-vclos env e))))

(define (whnf v)
  (cond ((vapp? v)
	 (let* ((u (vapp-func v))
		(w (vapp-arg v)))
	   (app (whnf u) (whnf v))))
	((vclos? v)
	 (let* ((env (vclos-env v))
		(e (vclos-expr v)))
	   (eval env e)))
	(else v)))

;; the conversion algorithm
(define (eq-val k u1 u2)
  (let* ((wu1 (whnf u1))
	 (wu2 (whnf u2)))
    (cond ((and (vtype? wu1)
		(vtype? wu2))
	   #t)
	  ((and (vapp? wu1)
		(vapp? wu2))
	   (let* ((t1 (vapp-func wu1))
		  (w1 (vapp-arg wu1))
		  (t2 (vapp-func wu2))
		  (w2 (vapp-arg wu2)))
	     (and (eq-val k t1 t2)
		  (eq-val k w1 w2))))
	  ((and (vgen? wu1)
		(vgen? wu2))
	   (let* ((k1 (vgen-int wu1))
		  (k2 (vgen-int wu2)))
	     (= k1 k2)))
	  ((and (and (vclos? wu1) (vabs? (vclos-expr wu1)))
		(and (vclos? wu2) (vabs? (vclos-expr wu2))))
	   (let* ((env1 (vclos-env wu1))
		  (x1 (vabs-func (vclos-expr wu1)))
		  (e1 (vabs-arg (vclos-expr wu1)))
		  (env2 (vclos-env wu2))
		  (x2 (vabs-func (vclos-expr wu2)))
		  (e2 (vabs-arg (vclos-expr wu2)))
		  (v (make-vgen k)))
	     (eq-val
	      (+ k 1)
	      (make-vclos (update env1 x1 v) e1)
	      (make-vclos (update env2 x2 v) e2))))
	  ((and (and (vclos? wu1) (pi? (vclos-expr wu1)))
		(and (vclos? wu2) (pi? (vclos-expr wu2))))
	   (let* ((env1 (vclos-env wu1))
		  (x1 (pi-id (vclos-expr wu1)))
		  (a1 (pi-expr1 (vclos-expr wu1)))
		  (b1 (pi-expr2 (vclos-expr wu1)))
		  (env2 (vclos-env wu2))
		  (x2 (pi-id (vclos-expr wu2)))
		  (a2 (pi-expr1 (vclos-expr wu2)))
		  (b2 (pi-expr2 (vclos-expr wu2)))
		  (v (make-vgen k)))
	     (and (eq-val k
			  (make-vclos env1 a1)
			  (make-vclos env2 a2))
		  (eq-val (+ k 1)
			  (make-vclos (update env1 x1 v) b1)
			  (make-vclos (update env2 x2 v) b2)))))
	  (else #f))))

;; type checking and type inference
(define (check-type k rho gamma e)
  (check-expr k rho gamma e (make-vtype)))

(define (check-expr k rho gamma e v)
  (cond ((abs? e)
	 (let* ((x (abs-id e))
		(n (abs-body e))
		(wv (whnf v)))
	   (cond ((and (vclos? wv)
		       (pi? (vclos-expr wv)))
		  (let* ((env (vclos-env wv))
			 (y (pi-id (vclos-expr wv)))
			 (a (pi-expr1 (vclos-expr wv)))
			 (b (pi-expr2 (vclos-expr wv)))
			 (v (make-vgen k)))
		    (check-expr (+ k 1)
				(update rho x v)
				(update gamma x (make-vclos env a))
				n
				(make-vclos (update env y v) b))))
		 (else (error wv "expected PI -- CHECK-EXPR")))))
	((pi? e)
	 (let* ((x (pi-id e))
		(a (pi-expr1 e))
		(b (pi-expr2 e))
		(wv (whnf v)))
	   (cond ((vtype? wv)
		  (and (check-type k rho gamma a)
		       (check-type (+ k 1)
				   (update rho x (make-vgen k))
				   (update gamma x (make-vclos rho a))
				   b)))
		 (else (error wv "expected TYPE -- CHECK-EXPR")))))
	((let? e)
	 (let* ((x (let-id e))
		(e1 (let-expr1 e))
		(e2 (let-expr2 e))
		(e3 (let-expr3 e)))
	   (and (check-type k rho gamma e2)
		(check-expr k
			    (update rho x (eval rho e1))
			    (update gamma x (eval rho e2))
			    e3
			    v))))
	(else (eq-val k (infer-expr k rho gamma e) v))))

(define (infer-expr k rho gamma e)
  (cond ((var? e) (lookup (var-id e) gamma))
	((app? e)
	 (let* ((e1 (app-func e))
		(e2 (app-arg e))
		(we1 (whnf (infer-expr k rho gamma e1))))
	   (cond ((and (vclos? we1) (pi? (vlcos-expr we1)))
		  (let* ((env (vclos-env we1))
			 (x (pi-id (vclos-expr we1)))
			 (a (pi-expr1 (vclos-expr we1)))
			 (b (pi-expr2 (vclos-expr we1)))
			 (env (vclos-env we1)))
		    (if (check-expr k rho gamma e2 (make-vclos env a))
			(make-vclos (update env x (make-vclos rho e2)) b)
			(error e2 "application error -- INFER-EXPR"))))
		 (else (error we1 "APP, expected PI -- INFER-EXPR")))))
	((type? e) (make-vtype))
	(else (error e "cannot infer type -- INFER-EXPR"))))

(define (tc m a)
  (and (check-type 0 '() '() a)
       (check-expr 0 '() '() m (make-vclos '() a))))
