;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: White language for reflective type checkers
;; Licensed under MIT

;; USER LEVEL 0 -> SIMPLY TYPED LAMBDA CALCULUS
;; META LEVEL 1 -> SYSTEM F/SYSTEM w (polymorphism)
;; META LEVEL 2 -> DEPENDENT TYPES
;; META LEVEL 3 -> HOMOTOPY TYPES

;; environment functions
(define the-empty-environment '())

;; expression
(define-structure expr v t)
