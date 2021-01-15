; fixed-point
(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))
    

;newtons-method
(define dx 0.00001)

(define (deriv g)
    (lambda (x)
        (/ (- (g (+ x dx)) (g x)) dx)))

(define (newtons-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
    (fixed-point (newtons-transform g) guess))


; ex1-40: cubic
(define (cubic a b c)
    (lambda (x)
        (+ (cube x) (* a (square x) (* b x) c))))

;(newtons-method (cubic a b c) 1)