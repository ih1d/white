;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Weakest precondition eval
;; Licensed under GPLv3

;; Evaluates the weakest precondition P
;; for statement S and postcondition R
(define wp
  (lambda (S R)
    (cond
     ((null? S) '())
     ((eq? (car S) 'skip)
      (rdce R))
     ((eq? (car S) 'abort)
      #f)
     ((eq? (car S) 'assignment)
      (let ((v (lookup (second S) R)))
	(if v
	    (rdce (substitute v (third S) R))
	    (error (second S) "variable does not exist."))))
     ((eq? (car S) 'seq)
      (wp (cadr S) (wp (caddr S) R)))
     (else
      (error (car S) "not defined")))))

;; Lookup for a variable v in R
(define lookup
  (lambda (v R)
    (cond
     ((null? R) #f)
     ((eq? v (car R)) v)
     ((list? (car R))
      (or (lookup v (car R))
	  (lookup v (cdr R))))
     (else
      (lookup v (cdr R))))))

;; Substitute variable v for expression e in R
(define substitute
  (lambda (v e R)
    (cond
     ((null? R) '())
     ((eq? v (car R))
      (cons e (cdr R)))
     ((list? (car R))
      (cons (substitute v e (car R))
	    (substitute v e (cdr R))))
     (else
      (cons (car R)
	    (substitute v e (cdr R)))))))

(define rdce
  (lambda (expr)
    (if (and (pair? expr)
	     (member (car expr) '(> < >= <= =)))
	(let* ((op (car expr))
	       (lhs (cadr expr))
	       (rhs (caddr expr)))
	  (cond
	   ((and (pair? lhs)
		 (eq? (car lhs) '-))
	    (let ((var (cadr lhs))
		  (const (caddr lhs)))
	      `(,op ,var ,(+ rhs const))))
	   
	   ((and (pair? lhs)
		 (eq? (car lhs) '+))
	    (let ((var (cadr lhs))
		  (const (caddr lhs)))
	      `(,op ,var ,(- rhs const))))
	   
	   ((and (pair? lhs)
		 (eq? (car lhs) '*))
	    (let ((coeff (cadr lhs))
		  (var (caddr lhs)))
	      (if (not (= coeff 0))
		  `(,op ,var ,(/ rhs coeff))
		  'error)))))
	expr)))
