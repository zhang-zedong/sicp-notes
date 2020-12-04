; chapter1 exercise
; Exercise 1.3
; Define a procedure that takes three numbers as arguments 
; and returns the sum of the squares of the two larger numbers.
(define (square a) (* a a))
(define (max a b)
    (cond
        ((> a b) a)
        (else b)
    )
)

(define (min a b)
    (cond
        ((> a b) b)
        (else a)
    )
)

(define (sp-sum a b c)
    (+ (square (max a b)) (square (max (min a b) c) ) )
)