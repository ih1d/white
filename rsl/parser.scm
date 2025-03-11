;; Author: Isaac H. Lopez Diaz
;; Description: Main parser module

;; Parser main structure
(define-structure Parser input either)

;; Either struct
(define-structure Either left right)

;; Main parser function
(define parser
  (lambda (f i)
    (let ((p (make-Parser i (make-Either '() '()))))
      (f p))))

;; given a predicate return true if it worked and modify the stream
;; else return false with an error
(define satisfy
  (lambda (pred)
    (lambda (p)
      (cond
       ((null? (Parser-input p))
	(set-Parser-either! p (make-Either (list 'eof) '()))
	(Either-left (Parser-either p)))
       ((pred (car (Parser-input p)))       
	(set-Parser-either! p (make-Either '() (list (car (Parser-input p)) (cdr (Parser-input p)))))
	(Either-right (Parser-either p)))
       (else
	(set-Parser-either! p (make-Either (list 'unexpected
					       (car (Parser-input p)))
					 '()))
	(Either-left (Parser-either p)))))))

;; Given a char match if the current stream satisfies the condition
(define char
  (lambda (c)
    (satisfy (lambda (i) (char=? i c)))))

;; Check if the next character is a digit
(define digit
  (lambda ()
    (satisfy (lambda (i) (char-numeric? i)))))
