#lang racket/gui

; Import the neccesary libraries
(require racket/gui
         racket/draw)

(require "../GUI/drawer.rkt")
(require "../graphManagement/graphCreator.rkt")
(require "../graphManagement/pathFinder.rkt")

(provide run)


; Create some data structures that will be use then
(define coords-list '())
(define edges-list '())
(define graph '())
(define weight "      ")


; Define graphically the types of edges that will be used in the graphs 
(define oneWay (make-object pen% "GAINSBORO" 4 'solid))
(define twoWay (make-object pen% "DIM GRAY" 4 'solid))
(define blackPen (make-object pen% "BLACK" 2 'solid))
(define whitePen (make-object pen% "SNOW" 1 'solid))
(define GreenPen (make-object pen% "LIGHT BLUE" 2 'solid))

; Create the bitmap with the lenght given
(define bitmap-blank
  (lambda [[w 0] [h #false] #:backing-scale [backing-scale 2.0]]
    (define width  (max 1 (exact-ceiling w)))
    (define height (max 1 (exact-ceiling (or h w))))
    (make-bitmap width height #:backing-scale backing-scale)))


; Adjust the size of the bitmap in the independent directions according to the given scale
(define bitmap-scale
  (case-lambda
    [(bmp scale)
     (if (= scale 1.0) bmp (bitmap-scale bmp scale scale))]
    [(bmp scale-x scale-y)
     (cond [(and (= scale-x 1.0) (= scale-y 1.0)) bmp]
           [else (let ([w (max 1 (exact-ceiling (* (send bmp get-width) scale-x)))]
                       [h (max 1 (exact-ceiling (* (send bmp get-height) scale-y)))])
                   (define dc (make-object bitmap-dc% (bitmap-blank w h)))
                   (send dc set-smoothing 'aligned)
                   (send dc set-scale scale-x scale-y)
                   (send dc draw-bitmap bmp 0 0)
                   (or (send dc get-bitmap) (bitmap-blank)))])]))


; Define the main frame for Wazitico
(define mainFrame (new frame% [label "Wazitico"]
                   [width 300]
                   [height 300]
                   [style '(no-resize-border)]))

(define mainPanel (new vertical-panel% [parent mainFrame]
                                         [alignment '(center center)]
                                         [spacing 15])) 



; Create a welcome label
(define welcome-label (new message% [parent mainPanel]
                                  [label "¡Bienvenido a Wazitico!"]
                                  [font (make-font #:size 16)]
                                  ))


; Creates buttons for selecting language
(new button% [parent mainPanel]
             [label "Iniciar"]
             [min-width 100] 
             [min-height 60] 
             [vert-margin 10]
             [horiz-margin 5]
             [font (make-font #:size 14)]
             [callback (lambda (button event)
                         (send mainFrame show #f)
                         (send cityFrame show #t))])


; Create the main interactive frame
(define cityFrame (new frame% [label "Wazitico"]
                   [width 800]
                   [height 600]
                   [style '(no-resize-border)]
                   [alignment '(center center)]))

; Create a horizontal panel in cityframe
(define cityPanel (new horizontal-panel% [parent cityFrame] ))

; Create a vertical panel in cityPanel
(define ButtonPanel (new vertical-panel% [parent cityPanel]
                                         [alignment '(center center)]
                                         [spacing 30])) 

; Create a canvas in cityPanel
(define map-canvas (new canvas% [parent cityPanel]
                       [style '(border)]
                       [vert-margin 10]  
                       [horiz-margin 10]
                       [min-height 600]
                       [min-width 900]                       
                       ))
; Create an object to draw in the new canvas
(define dc (send map-canvas get-dc))

; Create a grid in the canvas
(define (draw-grid-in-canvas canvas cell-width cell-height)
  (define dc (send canvas get-dc))
  (send dc set-pen GreenPen) ; Puedes ajustar el color y estilo de la cuadrícula según tus preferencias
  

  (define canvas-width (send canvas get-width))
  (define canvas-height (send canvas get-height))


  (define (draw-horizontal-lines y)
    (send dc draw-line 0 y canvas-width y)
    (send dc set-font (make-font #:size 10)) 
    (send dc draw-text (format "~a" y) 5 (- y 20))) 

 
  (define (draw-vertical-lines x)
    (send dc draw-line x 0 x canvas-height)
    (send dc set-font (make-font #:size 10)) 
    (send dc draw-text (format "~a" x) (+ x 5) 5))

  
  (for ([y (in-range 0 canvas-height cell-height)])
    (draw-horizontal-lines y))

  
  (for ([x (in-range 0 canvas-width cell-width)])
    (draw-vertical-lines x)))


; Create another vertical panel in cityPanel
(define InformationPanel (new vertical-panel% [parent cityPanel]
                                         [alignment '(center center)]
                                         [min-width 130]
                                         [spacing 30])) 

; Checks if the node list is empty
(define (check-nodes-list)
  (cond
    [(empty? coords-list)
     ]
    [else
     (draw-all-nodes)]))

; Create values for the lenght of the buttons
(define button-width 100) 
(define button-height 40) 


; Create the button to add city, which is located in ButtonPanel
(define addCityButton (new button% [parent ButtonPanel]
             [label "Nueva Ciudad"]
             [min-width button-width]
             [min-height button-height]
             [vert-margin 10]  
             [horiz-margin 5]
             [callback (lambda (button event)
                         (send addCityFrame show #t)
                         (draw-grid-in-canvas map-canvas 100 100)
                         (check-nodes-list)
                         )]))

; Create the button to add roads, which is located in ButtonPanel
(define addRoadButton (new button% [parent ButtonPanel]
             [label "Nueva Carretera"]
             [min-width button-width]
             [min-height button-height]
             [vert-margin 10]  
             [horiz-margin 5]
             [callback (lambda (button event)
                         (send roadFrame show #t)
        )]))

; Create the button to check routes, which is located in ButtonPanel
(define selectRouteButton (new button% [parent ButtonPanel]
             [label "Encontrar Ruta"]
             [min-width button-width]
             [min-height button-height]
             [vert-margin 10]  
             [horiz-margin 5]
             [callback (lambda (button event)
                         (send cityFrame show #t)
                         (cond((not(null? coords-list))
                               (clearCanvas)))
                         (send tripFrame show #t))]))

; Create the button to clear nodes, which is located in ButtonPanel
(define clearNodesButton (new button% [parent ButtonPanel]
             [label "Limpiar Mapa"]
             [min-width button-width]
             [min-height button-height]
             [vert-margin 10]  
             [horiz-margin 5]
             [callback (lambda (button event)
                         (clearAllNodes)
                         )]))
; Clear all nodes
(define (clearAllNodes)
  (set! coords-list '()) 
  (send dc clear) 
  (send dc flush) 
  )

; Clear the canvas
(define (clearCanvas)
  (send cityFrame show #t)
  (sleep/yield 0.01)
  (draw-all-nodes)
  (draw-all-edges edges-list)
  )


; Create a label in InformationPanel to display the shortest path
(define weightLabel (new message% [parent InformationPanel]
                          [font (make-font #:size 10)]
                          [label (string-append "Distancia mas corta:\n" "\n" weight)]))


; Create the lenghts for the secondary frames
(define popup-window-width 300)
(define popup-window-height 200)
(define text-field-max-width 150)

; Create "New City" popup window
(define addCityFrame (new frame% [label "Nueva Ciudad"]
                   [width popup-window-width]
                   [height popup-window-height]
                   [alignment '(center center)]
                   [style '(no-resize-border)]))


; Create vertical panel for "New City" popup
(define verticalCityPanel (new vertical-panel% [parent addCityFrame]
                     [alignment '(center center)]
                     [spacing 10]))


; Create text fields for adding a new city
(define addCityText ( new text-field% [parent verticalCityPanel]
                                    [label "Nombre de la ciudad:   "]
                                    [min-width 5]
                                    ))


(define addXText ( new text-field% [parent verticalCityPanel]
                                    [label "     Longitud:                   "]
                                    [min-width 5] ))



(define addYText ( new text-field% [parent verticalCityPanel]
                                    [label "      Latitud:                      "]
                                    [min-width 5] ))

; Create "Add" button for adding a new city
(define add-city-window-button (new button% [parent verticalCityPanel]
             [label "Añadir"]
             [callback (lambda (button event)
                         (define nodeName (send addCityText get-value))
                         (define xCoord (send addXText get-value))
                         (define yCoord (send addYText get-value))
                         (cond((not (or (equal? nodeName "") (equal? xCoord "") (equal? yCoord "")))
                               (set! coords-list (append coords-list (list(list nodeName xCoord yCoord))))
                               (set! graph (addNode nodeName graph))
                               (draw-node nodeName (string->number xCoord) (string->number yCoord))))
                         (send addCityFrame show #f)
                         )]))



; Create "New Road" popup window
(define roadFrame (new frame% [label "Nueva ruta"]
                   [width popup-window-width]
                   [height popup-window-height]
                   [alignment '(center center)]
                   [style '(no-resize-border)]))

; Create vertical panel for "New Road" popup
(define verticalPanelRoute (new vertical-panel% [parent roadFrame]
                     [alignment '(center center)]
                     [spacing 10]))


; Create text fields for adding a new road
(define roadInitialText (new text-field% [parent verticalPanelRoute]
                                    [label "    Origen:          "]
                                    [min-width 10]
                                    ))


(define roadFinalText ( new text-field% [parent verticalPanelRoute]
                                    [label "    Destino:        "]
                                    [min-width 10]
                                    ))


(define roadText (new text-field% [parent verticalPanelRoute]
                                    [label "Distancia(km): "]
                                    [min-width 10]
                                    ))
; Create check-box for specifying road type
(define wayCheck (new check-box%  
      [label "Unidireccional"]  
      [parent verticalPanelRoute]))



(define (city-exists? cityName)
  (ormap (lambda (city)
           (equal? cityName (car city)))
         coords-list))


; Create "Ok" button for adding a new road
(define add-road-window-button
  (new button%
       [parent verticalPanelRoute]
       [label "Añadir"]
       [callback (lambda (button event)
                   (define initialNode (send roadInitialText get-value))
                   (define finalNode (send roadFinalText get-value))
                   (define weight  (send roadText get-value))
                   (define isDirected? (send wayCheck get-value))
                   (if (not (city-exists? initialNode))
                       (print "Error: La ciudad de origen no existe")
                       (if (not (city-exists? finalNode))
                           (print "Error: La ciudad de destino no existe")
                           (begin
                             (set! edges-list (append edges-list (list (list initialNode finalNode weight isDirected?))))
                             (set! graph (addEdge initialNode finalNode (string->number weight) isDirected? graph))
                             (draw-edge initialNode finalNode weight isDirected?)
                             (send roadFrame show #f)
                             )
                           )
                       )
                   )]))


; Function to draw an edge on the canvas
(define (draw-edge initialNode finalNode weight isDirected?)
  (draw-line dc initialNode finalNode isDirected?)                                              
  (number-draw weight initialNode finalNode)
  )

; Function to draw all nodes on the canvas
(define (draw-all-edges edges-list)
  (cond ((not(null? edges-list))
         (draw-edge (caar edges-list) (cadar edges-list) (caddar edges-list) (cadr(cddar edges-list)))
         (draw-all-edges (cdr edges-list)))
         ))



; Function to draw all nodes on the canvas
(define (draw-all-nodes)
  (draw-all-nodes-aux coords-list)
  )


; Auxiliary function to draw all nodes
(define (draw-all-nodes-aux list)
  (cond ( (null? (cdr list) )
              (draw-node (caar list) (string->number (cadar list)) (string->number (caddar list)) )
           )
        (else
             (draw-node (caar list) (string->number (cadar list)) (string->number(caddar list)) )
             (draw-all-nodes-aux (cdr list))
         )))


; Function to draw a single node on the canvas
(define (draw-node node x y)
  (define radius 30) 
  (send dc set-brush (make-object brush% "MAGENTA" 'solid))
  (send dc set-pen blackPen)
  (send dc draw-ellipse (- x radius) (- y radius) (* 2 radius) (* 2 radius))
  (send dc set-pen blackPen)
  (send dc set-font (make-font #:size 14 #:weight 'bold))
  (send dc draw-text node (- x 40) (- y 55))) 




; Function to draw a line (edge) between two nodes
(define (draw-line dc origin destiny way)
  (cond ((equal? way #t) ; Una vía
         (send dc set-pen (make-object pen% "GAINSBORO" 6 'solid))) 
        ((equal? way #f)
         (send dc set-pen (make-object pen% "DIM GRAY" 6 'solid))) 
    )
  (send dc draw-line
        (getCoords origin "x") (getCoords origin "y")
        (getCoords destiny "x") (getCoords destiny "y")))


; Function to draw distance number on an edge
(define (number-draw weight origin destiny)
  (define x (/ (+ (getCoords origin "x") (getCoords destiny "x")) 2))
  (define y (/ (+ (getCoords origin "y") (getCoords destiny "y")) 2))
  (send dc set-font (make-font #:size 10)) 
  (send dc draw-text weight (- x 10) y)) 
  


; Function to get coordinates of a node
(define (getCoords nodo pos)
  (getCoordsAux nodo pos coords-list)
    )

; Auxiliary function to get coordinates of a node
(define (getCoordsAux nodo pos list)
  (cond( (null? list)
         -1)
       ( (equal? nodo (caar list))
         (cond( (equal? pos "x")
                (string->number(cadar list))
               )
              ( (equal? pos "y")
                (string->number(caddar list))
               )
              (else
               -1)
           )
        )
       (else
        (getCoordsAux nodo pos (cdr list))
        )
   )
)

; Function to get coordinates of nodes in a path
(define (getPathCoords path)
  (cond ((null? path)
         '())
        (else
         (append
          (list (list (getCoords (car path) "x")(getCoords (car path) "y")))
          (getPathCoords (cdr path))))))



; Create the "Find Route" frame
(define tripFrame (new frame% [label "Encontrar Route"]
                   [width popup-window-width]
                   [height popup-window-height]
                   [alignment '(center center)]
                   [style '(no-resize-border)]))


; Create vertical panel for "Find Route" frame
(define verticalPanelTrip (new vertical-panel% [parent tripFrame]
                     [alignment '(center center)]
                     [spacing 10]))

; Create text fields for specifying the route
(define routeOrigin_entry ( new text-field% [parent verticalPanelTrip]
                                    [label "Origen:   "]
                                    [min-width 5]
                                    ))

(define routeDestination_entry ( new text-field% [parent verticalPanelTrip]
                                    [label "Destino: "]
                                    [min-width 5]
                                    ))

; Create "Ok" button for finding a route
(new button% [parent verticalPanelTrip]
             [label "Ok"]
             [callback (lambda (button event)
                         (define paths (find_all_paths (send routeOrigin_entry get-value)
                                                       (send routeDestination_entry get-value)
                                                       graph))
                         (print paths)
                         (define shortestPath '())
                         (cond ((not(null? paths))
                               (set! shortestPath (min (cdr paths) (car paths) (path_distance (car paths) graph)))
                               (sleep/yield 0.5)
                               (draw_all_paths paths)
                               (draw_path_aux shortestPath "green")))
                         (send weightLabel set-label (string-append "Distancia más corta(km):\n" "\n" "           " (number->string (path_distance shortestPath graph))))
                         (send tripFrame show #f)
                         )])



; Function to find the minimum weighted path
(define (min paths pivot weight)
  (cond((null? paths)
        pivot)
       (else
        (cond ((< (path_distance (car paths) graph) weight)
               (min (cdr paths) (car paths) (path_distance (car paths) graph)))
              (else
               (min (cdr paths) pivot weight))))))


; Function to draw a specific path in a given color
(define (draw_path_aux path color)
  (set_pen color 5 dc)
  (draw_path (getPathCoords path) dc)
  )

; Function to draw all paths in the given list
(define (draw_all_paths paths)
  (cond ((not(null? paths))
         (draw_path_aux (car paths) "blue")
         (draw_all_paths (cdr paths)))))
         

; Function to run the GUI application
(define (run)
  (sleep/yield 1)
  (send mainFrame show #t))