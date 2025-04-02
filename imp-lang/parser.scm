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

