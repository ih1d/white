;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Main module
;; Licensed under MIT
(load "scanner.scm")
(load "parser.scm")

(define file->char-list
  (lambda (f)
    (call-with-input-file f
      (lambda (input-port)
	(let loop ((x (read-char input-port)))
	  (cond
	   ((eof-object? x) '())
	   (else (begin
		   (cons x
			 (loop (read-char input-port)))))))))))
(define main
  (lambda (f)
    (let ((chars (file->char-list f)))
      (let ((scanned (scanner chars)))
	(let ((parsed (expr scanned)))
	  parsed)))))
