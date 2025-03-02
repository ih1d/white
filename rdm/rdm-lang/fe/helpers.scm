;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Helper functions
;; Licensed under GPLv2

;; Take items from the list while pred is true
(define take-while
  (lambda (pred lat)
    (cond
     ((null? lat) '())
     ((pred (car lat))
      (cons (car lat)
	    (take-while pred (cdr lat))))
     (else '()))))


;; Drop items from list while pred is true
(define drop-while
  (lambda (pred lat)
    (cond
     ((null? lat) '())
     ((pred (car lat))
      (drop-while pred (cdr lat)))
     (else lat))))
