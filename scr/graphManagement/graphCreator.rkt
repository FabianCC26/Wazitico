#lang racket

; Provide the following functions
(provide addNode
         addAllNodes
         addEdge)

; Check if a node exists in the graph
(define (hasNode? node graph)
  (cond ((null? graph)
         #f)
        ((equal? node (caar graph))
         #t)
        (else
         (hasNode? node (cdr graph)))))

; Add a new node to the graph if it doesn't already exist
(define (addNode newNode graph)
  (cond ((not (hasNode? newNode graph))
         (append graph (list (list newNode '()))))
        (else
         graph)))

; Add all nodes in a list to the graph
(define (addAllNodes nodeList graph)
  (cond ((null? nodeList)
         graph)
        (else
         (addAllNodes (cdr nodeList) (addNode (car nodeList) graph)))))

; Check if an edge exists in the edges list of a node
(define (hasEdge? newNode edgesList)
  (cond ((null? edgesList)
         #f)
        ((equal? newNode (caar edgesList))
         #t)
        (else
         (hasEdge? newNode (cdr edgesList)))))

; Add an edge between two nodes with an optional weight and direction
(define (addEdge originNode endNode weight isDirected? graph)
  (cond ((and (hasNode? originNode graph) (hasNode? endNode graph))
         (cond (isDirected?
                (addEdgeAux originNode endNode weight graph))
               (else
                (addEdgeAux endNode originNode weight (addEdgeAux originNode endNode weight graph)))))
        (else
         '())))

; Auxiliary function to add an edge between two nodes to the graph
(define (addEdgeAux originNode endNode weight tempGraph)
  (cond ((null? tempGraph)
         '())
        ((equal? originNode (caar tempGraph))
         (cond ((not (hasEdge? endNode (cadar tempGraph)))
                (cond ((equal? (cadar tempGraph) '())
                       (append (list (list originNode (list (list endNode weight)))) (cdr tempGraph)))
                      (else
                       (append (list (list originNode (append (list (list endNode weight)) (cadar tempGraph)))) (cdr tempGraph)))))
               (else
                (append tempGraph (addEdgeAux originNode endNode weight '())))))
        (else
         (append (list (car tempGraph)) (addEdgeAux originNode endNode weight (cdr tempGraph))))))


  