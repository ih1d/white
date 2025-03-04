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
     ((char=? #\newline (car code)) (scanner-b (cdr code) (+ row 1) col))
     ((char=? #\space (car code)) (scanner-b (cdr code) row (+ col 1)))
     ((char=? #\+ (car code))
      (cons (token (car code) 'plus row (+ col 1))
	    (scanner-b (cdr code) row (+ col 1))))
     ((char=? #\- (car code))
      (let ((min&rst (scan-min code row col)))
	(cons (car min&rst)
	      (scanner-b (second min&rst)
			 (token->row (car min&rst))
			 (+ 1 (token->col (car min&rst)))))))
     ((char=? #\* (car code))
      (cons (token (car code) 'mult row (+ col 1))
	    (scanner-b (cdr code) row (+ col 1))))
     ((char=? #\~ (car code))
      (if (char=? #\> (second code))
	  (cons (token (list (car code) (second code)) 'partial-arrow row (+ col 2))
		(scanner-b (cdr (cdr code)) row (+ col 2)))
	  (report scanner "~" row col)))
     ((char-upper-case? (car code))
      (let ((type&rst (scan-type code row col)))
	(cons (car type&rst)
	      (scanner-b (second type&rst)
			 (token->row (car type&rst))
			 (+ 1 (token->col (car type&rst)))))))
     ((char-numeric? (car code))
      (let ((num&rst (scan-number code row col)))
	(cons (car num&rst)
	      (scanner-b (second num&rst)
			 (token->row (car num&rst))
			 (+ 1 (token->col (car num&rst))))))))))

;; helper to scan number and make token
(define scan-number
  (lambda (code row col)
    (let ((num (take-while char-numeric? code))
	  (rst (drop-while char-numeric? code)))
      (list (token num 'number row (+ col (length num)))
	    rst))))

;; helper to scan min, lambda or arrow
(define scan-min
  (lambda (code row col)
    (cond
     ((char=? #\> (second code))
      (list (token (list (car code) (second code)) 'arrow row (+ col 2))
	    (cdr (cdr code))))
     ((char=? #\\ (second code))
      (list (token (list (car code) (second code)) 'lambda row (+ col 2))
	    (cdr (cdr code))))
     (else
      (list (token (car code) 'minus row (+ col 1))
	    (cdr code))))))

;; helper to scan types
(define scan-type
  (lambda (code row col)
    (let ((type (take-while char-alphanumeric? code))
	  (rst (drop-while char-alphanumeric? code)))
      (list (token type 'type row (+ col (length type)))
	    rst))))

;; main struct of token
;; 4 tuple: (lexeme, cat, row-number, col-number)
(define token
  (lambda (lex cat row col)
    (list lex cat row col)))

;; get lexeme from token
(define token->lex
  (lambda (tok)
    (car tok)))

;; get category from token
(define token->cat
  (lambda (tok)
    (second tok)))

;; get row from token
(define token->row
  (lambda (tok)
    (third tok)))

;; get column from token
(define token->col
  (lambda (tok)
    (fourth tok)))
