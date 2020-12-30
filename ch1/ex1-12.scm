; Pascal's triangle
; Write a procedure that computes elements of Pascal's triangle by means of a recursive process.
; 行列从 0 计数

(define (pascal row column)
    (if (or (= column 0) (= column row))
        1
        (+  (pascal (- row 1) column) 
            (pascal (- row 1) (- column 1))))
)