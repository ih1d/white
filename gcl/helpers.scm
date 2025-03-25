;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Set of helper functions
;; Licensed under GPLv3

;; implies
(define implies
  (lambda (p q)
    (or (not p)
	q)))

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

      
