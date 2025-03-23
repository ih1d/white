;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Set of helper functions
;; Licensed under GPLv3

;; implies
(define implies
  (lambda (p q)
    (or (not p)
	q)))
