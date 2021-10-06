#lang sicp
(#%require sicp-pict)

; 50
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))
;(paint einstein)
;(paint (flip-vert einstein))
;(paint (flip-horiz einstein))
;(paint (rotate90 einstein))
;(paint (rotate180 einstein))
;(paint (rotate270 einstein))

; 51
(define (beside painter1 painter2)
  (let ((split-point (make-vect .5 0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0 0)
            split-point
            (make-vect 0 1)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1 0)
            (make-vect .5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-down
           (transform-painter
            painter1
            (make-vect 0 0)
            (make-vect 1 0)
            split-point))
          (paint-up
           (transform-painter
            painter2
            split-point
            (make-vect 1 .5)
            (make-vect 0 1))))
      (lambda (frame)
        (paint-down frame)
        (paint-up frame)))))
(paint (beside einstein einstein))
(paint (below einstein einstein))

(define (below1 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
(paint (below1 einstein einstein))
