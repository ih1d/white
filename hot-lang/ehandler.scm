;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Error handler module for interpreter
;; Licensed under GPLv2

;; error report for scanning/lexing
(define scanner-report
  (lambda (message row col)
    (error 'scanner
	   (string-append msg ", at line: "
			  (number->string row)
			  ", column: "
			  (number->string col)))))
