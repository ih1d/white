;; Author: Isaac H. Lopez Diaz
;; Description: Parser Combinators

;; Tuple structure
(define-structure Tuple fst snd)

;; If succeeds make a tuple
(define succeed
  (lambda (v)
    (lambda (inp)
      (cons (make-Tuple v (cdr inp))
	    '()))))

;; If fail make return empty list
(define fail
  (lambda (inp)
    '()))

;; Given a predicate, if it matches on the car of the input
;; then succeed else fail
(define satisfy
  (lambda (p)
    (lambda (inp)
      (cond
       ((null? inp)
	(fail '()))
       ((p (car inp))
	(succeed (car inp)))
       (else
	(fail (cdr inp)))))))

;; Given a char, see if matches on the car of the stream
(define literal
  (lambda (x)
    (satisfy
     (lambda (i) (char=? x i)))))

;; Given two combinators run both and match whichever succeeds
(define alt
  (lambda (p1 p2)
    (lambda (inp)
      (append (p1 inp)
	      (p2 inp)))))

;; Sequence two combinators
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

;; Transform the parsed value
(define using
  (lambda (p f)
    (lambda (inp)
      (let ((t (car (p inp))))
	(list
	 (make-Tuple (f (Tuple-fst t))
		     (Tuple-snd t)))))))


(define many
  (lambda (p)
    (alt (using (then p (many p))
		cons)
	 (succeed '() '()))))

(define string
  (lambda (inp)
    (if (null? inp)
	(succeed '())
	(using (then (literal (car inp))
		     (string (cdr inp)))
	       cons))))
