;; A Simple Reflective Interpreter

(define void
  (let ((g (con '* '*)))
    (lambda () g)))

(set! evaluate
      (lambda (e r k)
	((if (constant? e)
	     evaluate-constant
	     (if (variable? e)
		 evaluate-variable
		 (if (if? e)
		     evaluate-if
		     (if (assignment? e)
			 evaluate-assignment
			 (if (abstraction? e)
			     evaluate-abstraction
			     evaluate-combination)))))
	 e r k)))

(set! evaluate-constant
      (lambda (e r k)
	(k (constant-part e))))



