;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Parser for IMP Lang
;; Licensed under MIT
(load "scanner.scm")
(load "ehandler.scm")

;; arithmetic expression
(define arith-expr
  (lambda (tokens)
    (cond
     ((null? tokens) '())
     ((eq? 'number (car tokens))
      ;; do something
      )
     ((eq? 'plus (car tokens))
      ;; do something else
      )
     ((eq? )))))
