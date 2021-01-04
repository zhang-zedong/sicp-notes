(define (repeated f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))

(define (compose f g)
    (lambda (x)
        (f (g x))))

;fixed-point, average-damp

(define (average-damp f)
    (define (average a b)
        (/ (+ a b) 2))
    (lambda (x) (average (f x) x)))

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (good-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (good-enough? next guess)
                next
                (try next))))
    (try first-guess))

(define (sqrt x)
    (fixed-point (lambda (y) (/ x y)) 1.0 average-damp))


(define (exp a b)
        (if (= b 0) 
            1 
            (* a (exp a (- b 1)))))


; 开x的n次方根号，m为 average-damp 次数
(define (n-root x n m)
    (fixed-point ((repeated average-damp m)
                    (lambda (y) (/ x (exp y (- n 1)))))
                 1.0))


; (load "ex1-45.scm")

; (n-root 2 2)  wrong
; (fixed-point (lambda (x) (/ 2 x)) 1.0 average-damp)   yes
; (fixed-point (lambda (x) (/ 2 x)) 1.0 (repeated average-damp 2))  wrong
; (repeated average-damp 2) yes

; (n-root 2 2 1)