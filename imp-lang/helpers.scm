;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Helper functions for scanning and parsing
;; Licensed under MIT

;; take from list while predicate is true
(define take-while
  (lambda (pred lat)
    (cond
     ((null? lat) '())
     ((pred (car lat))
      (cons (car lat)
	    (take-while pred (cdr lat))))
     (else '()))))

;; drop from list while predicate is true
(define drop-while
  (lambda (pred lat)
    (cond
     ((null? lat) '())
     ((pred (car lat))
      (drop-while pred (cdr lat)))
     (else lat))))

;; take N items from list
(define take
  (lambda (n lat)
    (if (or (= n 0) (null? lat))
	'()
	(cons (car lat) (take (- n 1) (cdr lat))))))

;; drop N items from list
(define drop
  (lambda (n lat)
    (cond
     ((null? lat) '())
     ((= n 0) lat)
     (else (drop (- n 1) (cdr lat))))))

;; not whitespace predicate function
(define not-whitespace
  (lambda (char)
    (not (char-whitespace? char))))
