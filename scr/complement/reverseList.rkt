#lang racket

; Provide the reverse_all function
(provide reverse_all)

; Apply reverse to each sublist in a list
(define (reverse_all lst)
  (reverse_all_aux lst '())
)

(define (reverse_all_aux lst result)
  (cond ((null? lst) (reverse result))
        (else (reverse_all_aux (cdr lst)
                               (cons (reverse (car lst))
                                     result) ))
  )
)
