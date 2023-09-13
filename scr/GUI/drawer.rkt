#lang racket

; Import the necessary libraries
(require racket/gui
         racket/draw)

; Provide the following functions
(provide make_bitmap
         make_dc
         set_pen
         draw_path
         save)

; Create a bitmap with the given width and height
(define (make_bitmap w h)
  (define bm (make-bitmap w h))
  bm
)

; Create a device context (DC) with the given color, width, and bitmap
(define (make_dc color width bm)
  (define dc (new bitmap-dc% [bitmap bm]))
  (send dc set-pen color width 'solid)
  dc
)

; Set the pen color and width for a device context (DC)
(define (set_pen color width dc)
  (send dc set-pen color width 'solid)
  dc
)
  
; Draw a path represented by a list of nodes on the device context (DC)
(define (draw_path nodes dc)
  (draw_path_aux (car nodes) (cadr nodes) (cdr nodes) dc)
)

; Auxiliary function to draw a path by connecting nodes with lines
(define (draw_path_aux nodeA nodeB nodes dc)
  (cond ((<= (length nodes) 1) (draw_line nodeA nodeB dc))
        (else
         (draw_line nodeA nodeB dc)
         (draw_path_aux (car nodes) (cadr nodes) (cdr nodes) dc))
   )
)

; Draw a line between two points represented by nodeA and nodeB on the device context (DC)
(define (draw_line nodeA nodeB dc)
  (send dc draw-line
               (first nodeA) (last nodeA)
               (first nodeB) (last nodeB))
)

; Save a bitmap as an image with the given name (PNG format)
(define (save bm name)
  (send bm save-file name 'png))
