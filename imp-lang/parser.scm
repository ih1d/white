;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Parser for IMP Lang
;; Licensed under MIT
(load "scanner.scm")
(load "ehandler.scm")

;; arithmetic expressions
(define-structure aexp a)

;; boolean expressions
(define-structure bexp b)

;; locations
(define-structure loc X)

;; commands
(define-structure com c)

;; expressions
(define expression
  (lambda (tokens)
    (cond
     ((null? tokens) '())
     ((eq? 'plus (token-cat (car tokens)))
      (cons (make-aexp (cons (car tokens)
			     (term (cdr tokens))))))
     ((eq? 'minus (token-cat (car tokens)))
      (cons (make-aexp (cons (car tokens)
			     (term (cdr tokens))))))
     (else term tokens))))

;; terms
(define term
  (lambda (tokens)
    (cond
     ((null? tokens) '())
     ((eq? 'star (token-cat (car tokens)))
      (cons (make-aexp (cons (car tokens)
			     (factor (cdr tokens))))))
     (else factor tokens))))

;; factor
(define term
  (lambda (tokens)
    (cond
     ((null? tokens? ) '())
     ((eq? 'id (token-cat (car tokens)))
      (car tokens))
     ((eq? 'number (token-cat (car tokens)))
      (car tokens)))))
