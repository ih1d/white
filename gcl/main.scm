;; Author: Isaac H. Lopez Diaz <isaac.lopez@upr.edu>
;; Description: Main functio
;; Licensed under GPLv2
(load "scanner.scm")

(define (file->char-list path)
  (call-with-input-file path
    (lambda (input-port)
      (let loop ((x (read-char input-port)))
	(cond 
         ((eof-object? x) '())
         (#t (begin (cons x (loop (read-char input-port))))))))))

(define main
  (lambda (file)
    (map token-cat (scanner (file->char-list file)))))
