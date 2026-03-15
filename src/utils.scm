;; Author: Isaac H. Lopez Diaz
;; Description: Utility Functions

(define (take lst n)
  (if (= n 0)
      '()
      (cons (car lst)
	    (take (cdr lst)
		  (- n 1)))))

(define (drop lst n)
  (if (= n 0)
      lst
      (drop (cdr lst) (- n 1))))
	    
(define (iota n)
  (define (loop i)
    (if (= n i)
	'()
	(cons i (loop (+ i 1)))))
  (loop 0))

(define identity (lambda (x) x))
