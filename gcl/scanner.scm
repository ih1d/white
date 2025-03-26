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
     ((or (char=? #\space (car l))
	  (char=? #\tab (car l)))
      (scanner (cdr l)))
     ((char-alphabetic? (car l))
      (let ((e&rst (build-entry l)))
	(cons (car e&rst)
	      (scanner rst))))
     ((char-numeric? (car l))
      (let ((full-number (take-while char-numeric? l)))
	(cons (make-entry 'number full-number)
	      (scanner (drop-while char-numeric? l)))))
     ((char=? #\: (car l))
      (if (char=? #\= (cadr l))
	  (cons (make-entry 'assignment ":=")
		(scanner (cddr l)))
	  (cons (make-entry 'colon ":")
		(scanner (cdr l)))))
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
