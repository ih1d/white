;; Author: Isaac H. Lopez Diaz
;; Description: Parser Combinators

;; Tuple structure
(define-structure Tuple fst snd)

;; If succeeds make a tuple
(define succeed
  (lambda (v rst)
    (cons (make-Tuple v rst)
	  '())))

(define fail
  (lambda (inp)
    '()))

(define satisfy
  (lambda (p l)
    (cond
     ((null? l)
      (fail '()))
     ((p (car l))
      (succeed (car l)
	       (cdr l)))
     (else
      (fail (cdr l))))))

(define literal
  (lambda (x)
    (lambda (inp)
      (satisfy (lambda (i) (char=? x i))
	       inp))))

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
	   (make-Tuple
	    (make-Tuple (Tuple-fst t1)
			(Tuple-fst t2))
	    (Tuple-snd t2))))))))

(define using
  (lambda (p f)
    (lambda (inp)
      (let ((t (car (p inp))))
	(list
	 (make-Tuple (f (Tuple-fst t))
		     (Tuple-snd)))))))


