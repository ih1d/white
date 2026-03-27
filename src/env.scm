;; Author: Isaac H. Lopez Diaz
;; Description: Environment module
;; Licensed under GNU GPLv3

(define the-empty-environment '())

(define initial-environment
  (list (cons '+ +)
	(cons '- -)
	(cons '* *)
	(cons '/ /)))

(define (lookup var env)
  (if (pair? (assq var env))
      (cdr (assq var env))
      'nothing))
