;; Author: Isaac H. Lopez Diaz
;; Description: Store module (DAG)

;; Nodes
(define (make-node id kind) (cons id kind))

(define (node-id node) (car node))

(define (node-kind node) (cdr node))

(define (set-node-id! node id) (set-car! node id))

(define (set-node-kind! node kind) (set-cdr! node kind))

;; Edges
(define (make-edge node1 node2)
  (cons (node-id node1)
	(node-id node2)))

(define (fst edge) (car edge))

(define (snd edge) (cdr edge))

(define (set-fst! edge node)
  (set-car! edge (node-id node)))

(define (set-snd! edge node)
  (set-cdr! edge (node-id node)))

(define (edge=? edge1 edge2)
  (and (eq? (fst edge1)
	    (fst edge2))
       (eq? (snd edge1)
	    (snd edge2))))

;; DAG (a list of edges)
(define the-empty-DAG '())

(define (empty? DAG) (null? DAG))

(define (contains? DAG edge)
  (cond ((null? DAG) #f)
	((edge=? edge (car DAG)) #t)
	(else (contains? (cdr DAG) edge))))
