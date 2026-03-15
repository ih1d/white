;; Author: Isaac H. Lopez Diaz 
;; Description: Tensor module
(load "utils.scm")

(define (make-scalar scalar)
  (tensor (list scalar)))

(define (make-tensor-from-list ls)
  (define (flatten lst)
    (if (pair? lst)
	(apply append (map flatten lst))
	(list lst)))
  (let* ((shape (make-shape ls))
	 (stride (make-stride shape))
	 (rank (make-rank shape))
	 (data (flatten ls)))
    (list shape stride rank data)))

(define (make-tensor-from-vector v)
  (define (transform-all-lists ls)
    (cond ((null? ls) '())
	  ((vector? (car ls))
	   (cons (vector->list (car ls))
		 (transform-all-lists (cdr ls))))
	  (else ls)))
  (let ((ls (transform-all-lists (vector->list v))))
    (make-tensor-from-list ls)))

(define (make-rank shape) (length shape))

(define (make-shape ls)
  (if (pair? ls)
      (cons (length ls)
	    (make-shape (car ls)))
      '()))

(define (make-stride shape)
  (cdr (fold-right (lambda (d acc)
		     (cons (* d (car acc)) acc))
		   '(1)
		   shape)))

(define (tensor t)
  (cond ((list? t)
	 (make-tensor-from-list t))
	((vector? t)
	 (make-tensor-from-vector t))
	((number? t)
	 (make-scalar t))
	(else (error t "cannot make a tensor from that object"))))

(define (tensor? obj)
  (eq? 'tensor (car obj)))

(define (reshape flat shape)
  (if (null? (cdr shape))
      flat
      (let ((chunk (apply * (cdr shape))))
	(map (lambda (i)
	       (reshape (take (drop flat (* i chunk)) chunk) (cdr shape)))
	     (iota (car shape))))))

(define (flat-slice flat shape strides index)
  (let* ((offset (apply + (map * index strides)))
         (remaining-shape (list-tail shape (length index)))
         (size (apply * remaining-shape)))
    (if (null? remaining-shape)
        (list-ref flat offset)         
        (reshape (take (drop flat offset) size) remaining-shape))))

(define (tensor-ref t i)
  (let* ((shape (tensor-shape t))
         (stride (tensor-stride t))
         (data (tensor-data t))
         (result (flat-slice data shape stride i)))
    (tensor result)))

(define list->tensor make-tensor-from-list)

(define vector->tensor make-tensor-from-vector)

(define (tensor-shape t) (car t))

(define (tensor-stride t) (cadr t))

(define (tensor-rank t) (caddr t))

(define (tensor-data t) (cadddr t))
