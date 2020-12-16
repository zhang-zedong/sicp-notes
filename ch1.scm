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


; 1.1.7 Example
; Changed the program to the answer of Exercise 1.7.
(define (sqrt-iter guess last x)
    (if (good-enough? guess last)
        guess
        (sqrt-iter (improve guess x) guess x)))
    
;(define (good-enough? guess x)
;    (if (< (abs (- (square guess) x)) 0.001) #t #f) )
(define (good-enough? guess last)
    (if (< (abs (- guess last)) 0.001) #t #f))

(define (improve guess x)
    (/ (+ x (square guess)) (* 2 guess) ) )

; 第一次只要让这个1.0 不通过good-enough？就好，所以随意将第二个参数写10
(define (sqrt x)
    (sqrt-iter 1.0 10 x))

(define (abs x)
    (if (< x 0) (- x) x))
