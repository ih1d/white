;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Scanner for GCL
;; Licensed under GPLv2
(load "helpers.scm")

;; entry structure
(define-structure entry type lexeme)

;; Main scanner function
(define scanner
  (lambda (l)
    (cond
     ((null? l) '())
     ((char-number? (car l))
      (let ((full-number (take-while number? l)))
	(cons (make-entry 'number full-number)
	      (scanner (drop-while number? l)))))
     ((
     ((char=? #\+ (car l))
      (cons (make-entry 'plus (car l))
	    (scanner (cdr l))))
     ((char=? #\- (car l))
      (cons (make-entry 'minus (car l))
	    (scanner (cdr l))))
     ((char=? #\* (car l))
      (cons (make-entry 'mult (car l))
	    (scanner (cdr l))))
     ((char=? #\/ (car l))
      (cons (make-entry 'div (car l))
	    (scanner (cdr l))))
     (else
      (error (car l) "not defined")))))
