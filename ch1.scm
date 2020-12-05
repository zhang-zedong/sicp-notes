; chapter1 exercise
; Exercise 1.3
; Define a procedure that takes three numbers as arguments 
; and returns the sum of the squares of the two larger numbers.
(define (square a) (* a a))

(define (max a b)
    (cond
        ((> a b) a)
        (else b)))

(define (min a b)
    (cond
        ((> a b) b)
        (else a)))

(define (sum-special a b c)
    (+ (square (max a b)) (square (max (min a b) c) ) ))

; 1.1.7 Example: Square Roots by Newton's Method
(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
    
(define (good-enough? guess x)
    (if (< (abs (- (square guess) x)) 0.0001) #t #f) )

(define (improve guess x)
    (/ (+ x (square guess)) (* 2 guess) ) )

(define (sqrt x)
    (sqrt-iter 1.0 x))

(define (abs x)
    (if (< x 0) (- x) x))
