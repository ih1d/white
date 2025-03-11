;; Author: Isaac H. Lopez Diaz
;; Description: Parser Combinators

;; Tuple structure
(define-structure Tuple fst snd)

;; If succeeds make a tuple
(define succeed
  (lambda (v inp)
    (cons (make-Tuple v inp)
	  '())))

(define fail
  (lambda (inp)
    '()))

(define satisfy
  (lambda (p)
    (lambda (l)
      (cond
       ((null? l)
	(fail '()))
       ((p (car l))
	(succeed (car l)
		 (cdr l)))
       (else
	(fail (cdr l)))))))

(define literal
  (lambda (x)
    (satisfy (lambda (i) (char=? i x)))))

(define alt
  (lambda (p1 p2)
    (lambda (inp)
      (append (p1 inp)
	      (p2 inp)))))

(define then
  (lambda (p1 p2)
    (lambda (inp)
      (let ((t1 (car (p1 inp))))
	(let ((t2 (car (p2 (Tuple-snd t1)))))
	  (list
	   (make-tuple
	    (make-Tuple (Tuple-fst t1)
		        (Tuple-fst t2))
	   (Tuple-snd t2))))))))
