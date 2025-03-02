;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Lexer for RSL
;; Licensed under GPL2
(load "helpers.scm")

;; Performs lexical analysis of input
(define lexer
  (lambda (code)
    (cond
     ((null? code) '())
     ((char-numeric? (car code))
      (cons
       (make-token 'number
		   (take-while char-numeric? code))
       (lexer (drop-while char-numeric? code))))
     (else (error 'parsing
		  (string-append "found: "
				 (char->name (car code))))))))

;; Make a pair of tokens
(define make-token
  (lambda (tok str)
    (cons tok str)))
