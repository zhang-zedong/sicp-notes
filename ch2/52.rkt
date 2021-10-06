#lang sicp
(#%require sicp-pict)

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
    (make-segment (make-vect .45 .4) (make-vect .55 .4))
    (make-segment (make-vect .45 .7) (make-vect .55 .7)))))

(define (split compose-large compose-small)
  (define (recur painter n)
    (if (= n 0)
        painter
        (let ((smaller (recur painter (- n 1))))
          (compose-large painter (compose-small smaller smaller)))))
  recur)

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left up)
              (bottom-right right)
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl (rotate270 painter)) (tr (rotate90 painter))))
          (bottom (beside (bl (rotate90 painter)) (br (rotate270 painter)))))
      (below bottom top))))
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


(paint wave)
(paint (corner-split wave 3))
(paint (square-limit einstein 2))