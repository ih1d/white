;; Author: Isaac H. Lopez Diaz
;; Description: Main parser module

;; Main parser function
(define parser
  (lambda (f i)
    (f i)))

;; given a predicate return true if it worked and modify the stream
;; else return false with an error
(define satisfy
  (lambda (pred)
    (lambda (stream)
      (cond
       ((null? stream)
	(list 'eof))
       ((pred (car stream))
	(list (car stream)
	      (cdr stream)))
       (else
	(error (car stream) "unexpected"))))))

;; Given a char match if the current stream satisfies the condition
(define char
  (lambda (c)
    (satisfy (lambda (i) (char=? i c)))))
