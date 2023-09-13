#lang racket/gui

; Import the neccesary libraries
(require racket/gui
         racket/draw)

(require "../GUI/drawer.rkt")
(require "../graphManagement/graphCreator.rkt")
(require "../graphManagement/pathFinder.rkt")

(provide run
         logo_path)

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

; Create the bitmaps with the given images
(define logo (make-object bitmap% "../../assets/logo.png"))
(define logou (make-object bitmap% "../../assets/logounder.png"))

; Create the images by concatenating them with the directions
(define (logo_path path)
  (set! logo (make-object bitmap% (string-append path "logo.png")))
  (set! logou (make-object bitmap% (string-append path "logounder.png")))
  )

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


; Define the fill color of the shapes
(define lightblue-brush (make-object brush% "LIGHTBLUE" 'solid))




; Define the main frame for Wazitico
(define mainFrame (new frame% [label "Wazitico"]
                   [width 300]
                   [height 300]
                   [style '(no-resize-border)]))


; Create the button to start
(new button% [parent mainFrame]
             [label "Start"]
             [callback (lambda (button event)
                         (send mainFrame show #f)
                         (send cityFrame show #t))])




; Create the botton to add a new city
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
                       [min-height 500]
                       [min-width 870]                       
                       ))
; Create an object to draw in the new canvas
(define dc (send map-canvas get-dc))


; Create another vertical panel in cityPanel
(define InformationPanel (new vertical-panel% [parent cityPanel]
                                         [alignment '(center center)]
                                         [min-width 130]
                                         [spacing 30])) 

; Create values for the lenght of the buttons
(define button-width 100) 
(define button-height 40) 

; Create the button to add city, which is located in ButtonPanel
(define addCityButton (new button% [parent ButtonPanel]
             [label "New City"]
             [min-width button-width]
             [min-height button-height]
             [vert-margin 10]  
             [horiz-margin 5]
             [callback (lambda (button event)
                         (send addCityFrame show #t)
                         )]))

; Create the button to add roads, which is located in ButtonPanel
(define addRoadButton (new button% [parent ButtonPanel]
             [label "New Road"]
             [min-width button-width]
             [min-height button-height]
             [vert-margin 10]  
             [horiz-margin 5]
             [callback (lambda (button event)
                         (send roadFrame show #t)
        )]))

; Create the button to check routes, which is located in ButtonPanel
(define selectRouteButton (new button% [parent ButtonPanel]
             [label "Find Route"]
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
             [label "Clear Map"]
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
                          [label (string-append "Shortest Distance:\n" "\n" weight)]))


; Create the lenghts for the secondary frames
(define popup-window-width 300)
(define popup-window-height 200)
(define text-field-max-width 150)


(define addCityFrame (new frame% [label "New City"]
                   [width popup-window-width]
                   [height popup-window-height]
                   [alignment '(center center)]
                   [style '(no-resize-border)]))


(define verticalCityPanel (new vertical-panel% [parent addCityFrame]
                     [alignment '(center center)]
                     [spacing 10]))


(define addCityText ( new text-field% [parent verticalCityPanel]
                                    [label "City Name:   "]
                                    [min-width 5]
                                    ))


(define addXText ( new text-field% [parent verticalCityPanel]
                                    [label "Latitude:       "]
                                    [min-width 5] ))



(define addYText ( new text-field% [parent verticalCityPanel]
                                    [label "Longitude:   "]
                                    [min-width 5] ))


(define add-city-window-button (new button% [parent verticalCityPanel]
             [label "Add"]
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






(define roadFrame (new frame% [label "New Route"]
                   [width popup-window-width]
                   [height popup-window-height]
                   [alignment '(center center)]
                   [style '(no-resize-border)]))


(define verticalPanelRoute (new vertical-panel% [parent roadFrame]
                     [alignment '(center center)]
                     [spacing 10]))


(define roadInitialText (new text-field% [parent verticalPanelRoute]
                                    [label "    Origin:        "]
                                    [min-width 10]
                                    ))


(define roadFinalText ( new text-field% [parent verticalPanelRoute]
                                    [label "    Destiny:      "]
                                    [min-width 10]
                                    ))


(define roadText (new text-field% [parent verticalPanelRoute]
                                    [label "Distance(km):"]
                                    [min-width 10]
                                    ))

(define wayCheck (new check-box%  
      [label "One way road"]  
      [parent verticalPanelRoute]))


(define add-road-window-button (new button% [parent verticalPanelRoute]
             [label "Ok"]
             [callback (lambda (button event)
                         (define initialNode (send roadInitialText get-value))
                         (define finalNode (send roadFinalText get-value))
                         (define weight  (send roadText get-value))
                         (define isDirected? (send wayCheck get-value))
                         (set! edges-list (append edges-list (list (list initialNode finalNode weight isDirected?))))
                         (set! graph (addEdge initialNode finalNode (string->number weight) isDirected? graph))
                         (draw-edge initialNode finalNode weight isDirected?)
                         (send roadFrame show #f)
                         )]))



(define (draw-edge initialNode finalNode weight isDirected?)
  (draw-line dc initialNode finalNode isDirected?)                                              
  (number-draw weight initialNode finalNode)
  )

(define (draw-all-edges edges-list)
  (cond ((not(null? edges-list))
         (draw-edge (caar edges-list) (cadar edges-list) (caddar edges-list) (cadr(cddar edges-list)))
         (draw-all-edges (cdr edges-list)))
         ))







(define (draw-all-nodes)
  (draw-all-nodes-aux coords-list)
  )


(define (draw-all-nodes-aux list)
  (cond ( (null? (cdr list) )
              (draw-node (caar list) (string->number (cadar list)) (string->number (caddar list)) )
           )
        (else
             (draw-node (caar list) (string->number (cadar list)) (string->number(caddar list)) )
             (draw-all-nodes-aux (cdr list))
         )))



(define (draw-node node x y)
  (define radius 30) 
  (send dc set-brush (make-object brush% "MAGENTA" 'solid))
  (send dc set-pen blackPen)
  (send dc draw-ellipse (- x radius) (- y radius) (* 2 radius) (* 2 radius))
  (send dc set-pen blackPen)
  (send dc set-font (make-font #:size 14 #:weight 'bold))
  (send dc draw-text node (- x 40) (- y 55))) 









(define (draw-line dc origin destiny way)
  (cond ((equal? way #t) ; Una vía
         (send dc set-pen (make-object pen% "GAINSBORO" 6 'solid))) 
        ((equal? way #f)
         (send dc set-pen (make-object pen% "DIM GRAY" 6 'solid))) 
    )
  (send dc draw-line
        (getCoords origin "x") (getCoords origin "y")
        (getCoords destiny "x") (getCoords destiny "y")))


(define (number-draw weight origin destiny)
  (define x (/ (+ (getCoords origin "x") (getCoords destiny "x")) 2))
  (define y (/ (+ (getCoords origin "y") (getCoords destiny "y")) 2))
  (send dc set-font (make-font #:size 10)) 
  (send dc draw-text weight (- x 10) y)) 
  



(define (getCoords nodo pos)
  (getCoordsAux nodo pos coords-list)
    )


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

(define (getPathCoords path)
  (cond ((null? path)
         '())
        (else
         (append
          (list (list (getCoords (car path) "x")(getCoords (car path) "y")))
          (getPathCoords (cdr path))))))




(define tripFrame (new frame% [label "Find Route"]
                   [width popup-window-width]
                   [height popup-window-height]
                   [alignment '(center center)]
                   [style '(no-resize-border)]))

(define verticalPanelTrip (new vertical-panel% [parent tripFrame]
                     [alignment '(center center)]
                     [spacing 10]))

(define routeOrigin_entry ( new text-field% [parent verticalPanelTrip]
                                    [label "Origin:          "]
                                    [min-width 5]
                                    ))


(define routeDestination_entry ( new text-field% [parent verticalPanelTrip]
                                    [label "Destination: "]
                                    [min-width 5]
                                    ))


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
                         (send weightLabel set-label (string-append "Shortest Distance:\n" "\n" "           " (number->string (path_distance shortestPath graph))))
                         (send tripFrame show #f)
                         )])

(define (min paths pivot weight)
  (cond((null? paths)
        pivot)
       (else
        (cond ((< (path_distance (car paths) graph) weight)
               (min (cdr paths) (car paths) (path_distance (car paths) graph)))
              (else
               (min (cdr paths) pivot weight))))))



(define (draw_path_aux path color)
  (set_pen color 5 dc)
  (draw_path (getPathCoords path) dc)
  )

(define (draw_all_paths paths)
  (cond ((not(null? paths))
         (draw_path_aux (car paths) "blue")
         (draw_all_paths (cdr paths)))))
         


(define (run)
  (sleep/yield 1)
  (send mainFrame show #t))
