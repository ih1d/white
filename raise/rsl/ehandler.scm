;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Error handler module for interpreter
;; Licensed under GPLv2

;; error report for scanning/lexing
(define report
  (lambda (func msg row col)
    (error func
	   (string-append msg " found at line: "
			  (number->string row)
			  ", column: "
			  (number->string col)))))
