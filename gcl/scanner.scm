;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Scanner for GCL
;; Licensed under GPLv2
(load "helpers.scm")

;; entry structure
(define-structure entry type lexeme row col)

;; Main scanner function
(define scanner
  (lambda (l)
    (scanner-b l 0 0)))
(define scanner-b
  (lambda (l r c)
    (cond
     ((null? l) '())
     ((or (char=? #\space (car l))
	  (char=? #\tab (car l)))
      (scanner-b (cdr l) r (add1 c)))
     ((char=? #\newline (car l))
      (scanner-b (cdr l) (add1 r) 0))
     ((char-numeric? (car l))
      (let ((full-number (take-while char-numeric? l)))
	(cons (make-entry 'number (list->string full-number) r (+ c (length full-number)))
	      (scanner-b (drop-while char-numeric? l) r (+ c 1 (length full-number))))))
     ((char=? #\: (car l))
      (if (char=? #\= (cadr l))
	  (cons (make-entry 'assignment ":=" r (add1 (add1 c)))
		(scanner-b (cddr l) r (add1 (add1 c))))
	  (cons (make-entry 'colon ":" r (add1 c))
		(scanner-b (cdr l) r (add1 c)))))
     ((char=? #\+ (car l))
      (cons (make-entry 'plus "+" r (add1 c))
	    (scanner-b (cdr l) r (add1 c))))
     ((char=? #\- (car l))
      (cons (make-entry 'minus "-" r (add1 c))
	    (scanner-b (cdr l) r (add1 c))))
     ((char=? #\* (car l))
      (cons (make-entry 'mult "*" r (add1 c))
	    (scanner-b (cdr l) r (add1 c))))
     (else
      (error (car l) "not defined")))))
