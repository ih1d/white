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

;; Scanner-b helper
(define scanner-b
  (lambda (l r c)
    (cond
     ((null? l) '())
     ((or (char=? #\space (car l))
	  (char=? #\tab (car l)))
      (scanner-b (cdr l) r (add1 c)))
     ((char=? #\newline (car l))
      (scanner-b (cdr l) (add1 r) 0))
     ((char-alphabetic? (car l))
      (let ((e&rst (get-word l r c)))
	(let ((e (car e&rst))
	      (rst (cdr e&rst)))
	  (cons e
		(scanner-b rst (entry-row e) (entry-col e))))))
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


;; get keyword ord identifier
(define get-word
  (lambda (l r c)
    (let ((word (list->string (take-while char-alphanumeric? l))))
      (cond
       ((string=? "skip" word)
	(cons (make-entry 'skip "skip" r (+ c 4))
	      (drop-while char-alphanumeric? l)))
       ((string=? "abort" word)
	(cons (make-entry 'abort "abort" r (+ c 5))
	      (drop-while char-alphanumeric? l)))
       ((string=? "if" word)
	(cons (make-entry 'if "if" r (+ c 2))
	      (drop-while char-alphanumeric? l)))
       ((string=? "then" word)
	(cons (make-entry 'then "then" r (+ c 4))
	      (drop-while char-alphanumeric? l)))
       ((string=? "else" word)
	(cons (make-entry 'else "else" r (+ c 4))
	      (drop-while char-alphanumeric? l)))
       ((string=? "while" word)
	(cons (make-entry 'while "while" r (+ c 5))
	      (drop-while char-alphanumeric? l)))
       ((string=? "do" word)
	(cons (make-entry 'do "do" r (+ c 2))
	      (drop-while char-alphanumeric? l)))
       ((string=? "end" word)
	(cons (make-entry 'end "end" r (+ c 3))
	      (drop-while char-alphanumeric? l)))
       ((string=? "done" word)
	(cons (make-entry 'done "done" r (+ c 4))
	      (drop-while char-alphanumeric?e l)))
       ((string=? "goto" word)
	(cons (make-entry 'goto "goto" r (+ c 4))
	      (drop-while char-alphanumeric? l)))
       (else
	(cons (make-entry 'id word r (+ c (string-length word)))
	      (drop-while char-alphanumeric? l)))))))
