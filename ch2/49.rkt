#lang sicp
(#%require sicp-pict)
; 2.49
(define a
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 0 1))
    (make-segment (make-vect 0 0) (make-vect 1 0))
    (make-segment (make-vect 0 1) (make-vect 1 1))
    (make-segment (make-vect 1 0) (make-vect 1 1)))))
(define b
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 1 0) (make-vect 0 1)))))
(define c
  (segments->painter
   (list
    (make-segment (make-vect 0 0.5) (make-vect 1 0.5))
    (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
    (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
    (make-segment (make-vect 0.5 0) (make-vect 0.5 1))
    (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
    (make-segment (make-vect 0.5 1) (make-vect 1 0.5)))))
(define wave
  (segments->painter
   (list
    (make-segment (make-vect .4 1) (make-vect .35 .8))
    (make-segment (make-vect .6 1) (make-vect .65 .8))
    (make-segment (make-vect .35 .8) (make-vect .4 .6))
    (make-segment (make-vect .65 .8) (make-vect .6 .6))
    (make-segment (make-vect .4 .6) (make-vect 0 .5))
    (make-segment (make-vect .6 .6) (make-vect 1 .5))
    (make-segment (make-vect .4 .5) (make-vect 0 .4))
    (make-segment (make-vect .6 .5) (make-vect 1 .4))
    (make-segment (make-vect .4 .5) (make-vect .3 0))
    (make-segment (make-vect .6 .5) (make-vect .7 0))
    (make-segment (make-vect .4 0) (make-vect .45 .4))
    (make-segment (make-vect .6 0) (make-vect .55 .4))
    (make-segment (make-vect .45 .4) (make-vect .55 .4)))))

(paint a)
(paint b)
(paint c)
(paint wave)