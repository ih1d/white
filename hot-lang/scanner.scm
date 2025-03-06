;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Descrtiption: Scanner for hot lang
(load "helpers.scm")
(load "ehandler.scm")

;; main scanner function
(define scanner
  (lambda (code)
    (scanner-b code 0 0)))

;; helper function for scanner
(define scanner-b
  (lambda (code row col)
    (cond
     ((null? code) '())
     ((char=? #\newline (car code)) (scanner-b (cdr code) (+ row 1) 0))
     ((char=? #\space (car code)) (scanner-b (cdr code) row (+ col 1)))
     ((char-lower-case? (car code))
      (let ((word (take-while (lambda (x) (and (not (char=? #\: x))
					       (not (char-whitespace? x))))
			      code))
	    (rst (drop-while (lambda (x) (and (not (char=? #\: x))
					       (not (char-whitespace? x))))
			      code)))
	(let ((tok (scan-keyword-or-ident word row col)))
	  (cons tok
		(scanner-b rst (token-row tok) (token-col tok))))))
     ((char=? #\> (car code))
      (cond
       ((char-whitespace? (second code))
	(cons (make-token (car code) 'greater row (+ col 1))
	      (scanner-b (cdr code) row (+ col 1))))
       ((char=? #\= (second code))
	(cons (make-token (list (car code) (second code)) 'greater-equal row (+ col 2))
	      (scanner-b (cdr (cdr code)) row (+ col 1))))
       ((char=? #\< (second code))
	(cons (make-token (list (car code) (second code)) 'product row (+ col 2))
	      (scanner-b (cdr (cdr code)) (row (+ col 2)))))
       (else
	(report scanner (char->name (car code))))))
     ((char=? #\: (car code))
      (if (not (char-whitespace? (second code)))
	  (report scanner (char->name (second code)) row col)
	  (cons (make-token (car code) 'colon row (+ col 1))
		(scanner-b (cdr (cdr code)) row (+ col 2)))))
     ((char=? #\+ (car code))
      (cons (make-token (car code) 'plus row (+ col 1))
	    (scanner-b (cdr code) row (+ col 1))))
     ((char=? #\- (car code))
      (let ((min&rst (scan-min code row col)))
	(cons (car min&rst)
	      (scanner-b (second min&rst)
			 (token-row (car min&rst))
			 (+ 1 (token-col (car min&rst)))))))
     ((char=? #\* (car code))
      (cons (make-token (car code) 'mult row (+ col 1))
	    (scanner-b (cdr code) row (+ col 1))))
     ((char=? #\~ (car code))
      (if (char=? #\> (second code))
	  (cons (make-token (list (car code) (second code)) 'partial-arrow row (+ col 2))
		(scanner-b (cdr (cdr code)) row (+ col 2)))
	  (report scanner "~" row col)))
     ((char-upper-case? (car code))
      (let ((type&rst (scan-type code row col)))
	(cons (car type&rst)
	      (scanner-b (second type&rst)
			 (token-row (car type&rst))
			 (+ 1 (token-col (car type&rst)))))))
     ((char-numeric? (car code))
      (let ((num&rst (scan-number code row col)))
	(cons (car num&rst)
	      (scanner-b (second num&rst)
			 (token-row (car num&rst))
			 (+ 1 (token-col (car num&rst))))))))))

;; helper to scan keyword or identifier
(define scan-keyword-or-ident
  (lambda (word-lst row col)
    (let ((word (list->string word-lst)))
	(cond
	 ((string=? word "type")
	  (make-token "type" 'type row (+ col 4)))
	 ((string=? word "if")
	  (make-token "if" 'if row (+ col 2)))
	 ((string=? word "then")
	  (make-token "then" 'then row (+ col 4)))
	 ((string=? word "else")
	  (make-token "else" 'else row (+ col 4)))
	 ((string=? word "true")
	  (make-token "true" 'true row (+ col 4)))
	 ((string=? word "false")
	  (make-token "false" 'false row (+ col 5)))
	 (else
	  (make-token word 'id row (+ col (string-length word))))))))

;; helper to scan number and make token
(define scan-number
  (lambda (code row col)
    (let ((num (take-while char-numeric? code))
	  (rst (drop-while char-numeric? code)))
      (list (make-token num 'number row (+ col (length num)))
	    rst))))

;; helper to scan min, lambda or arrow
(define scan-min
  (lambda (code row col)
    (cond
     ((char=? #\> (second code))
      (list (make-token (list (car code) (second code)) 'arrow row (+ col 2))
	    (cdr (cdr code))))
     ((char=? #\\ (second code))
      (list (make-token (list (car code) (second code)) 'lambda row (+ col 2))
	    (cdr (cdr code))))
     (else
      (list (make-token (car code) 'minus row (+ col 1))
	    (cdr code))))))

;; helper to scan types
(define scan-type
  (lambda (code row col)
    (let ((type (take-while char-alphanumeric? code))
	  (rst (drop-while char-alphanumeric? code)))
      (list (make-token type 'type row (+ col (length type)))
	    rst))))

;; main struct of token
;; 4 tuple: (lexeme, category, row-number, col-number)
(define-structure token lex cat row col)
