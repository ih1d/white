;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Parser for IMP Lang
;; Licensed under MIT
(load "scanner.scm")
(load "ehandler.scm")

;; arithmetic expressions
(define-structure aexp a atok)

;; boolean expressions
(define-structure bexp b btok)

;; locations
(define-structure loc X Xtok)

;; commands
(define-structure com c ctok)

(define expression
  (lambda (e tok)
    (make-aexp e tok)))

(define factor
  (lambda (e tok)
    (make-aexp e tok)))

(define term
  (lambda (e tok)
    (make-aexp e tok)))

(define parser
  (lambda (tokens)
    (cond
     ((null? tokens) '())
     ((eq? 'plus (token-cat (car tokens)))
      (cons (expression 'plus (car tokens))
	    (parser (cdr tokens))))
     ((eq? 'minus (token-cat (car tokens)))
      (cons (expression 'minus (car tokens))
	    (parser (cdr tokens))))
     ((eq? 'star (token-cat (car tokens)))
      (cons (term 'mult (car tokens))
	    (parser (cdr tokens))))
     ((eq? 'number (token-cat (car tokens)))
      (cons (factor 'number (car tokens))
	    (parser (cdr tokens))))
     (else
      (error parser
	     (token-lex (car tokens))
	     (token-row (car tokens))
	     (token-col (car tokens)))))))
	     
