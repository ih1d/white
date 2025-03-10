;; Author: Isaac H. Lopez Diaz
;; Description: Main parser module
(load "structs.scm")

;; Main parser function
(define parser
  (lambda (f i)
    (f i)))
;; Given a predicate return a parser
(define satisfy
  (lambda (pred)
    (lambda (stream)
      (cond
       ((null? stream) (list 'eof))
       ((pred (car stream))
	(make-either '() (make-tuple (car stream)
				     (cdr stream))))
       (else
	(make-either (error 'unexpected (char->name (car stream))) '()))))))

;; Given a char cheq if the stream matches it
(define char
  (lambda (c)
    (satisfy (lambda (i) (char=? i c)))))
