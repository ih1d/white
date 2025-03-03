;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Descrtiption: Scanner for hot lang
(load "helpers.scm")

;; main scanner function
(define scanner
  (lambda (code)
    (lexer-b code 0 0)))

;; helper function for scanner
(define scanner-b
  (lambda (code row col)
    (cond
     ((null? code) '())
     ((char=? #\newline (car code)) (lexer-b (cdr code) (+ row 1) col))
     ((char-whitespace? (car code)) (lexer-b (cdr code) row (+ col 1)))
     ((char-numeric? (car code))
      (let ((num&rst (scan-number code row col)))
	(let ((tok
	    (tok (car num&rst))
	    (rst (second num&rst)))
	(cons tok
	      (lexer-b rst (token->row tok) (+ 1 (token->col tok)))))))))

;; helper to scan number and make token
(define scan-number
  (lambda (code row col)
    (let ((num (take-while char-numeric? code))
	  (rst (drop-while char-numeric? code)))
      (cons (token num 'number row (+ col (length num)))
	    rst))))

;; main struct of token
;; 4 tuple: (lexeme, cat, row-number, col-number)
(define token
  (lambda (lex cat row col)
    '(lex cat row col)))

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
