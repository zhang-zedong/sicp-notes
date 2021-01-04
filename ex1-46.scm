; good-enough? 按照题目要求，应该只有一个参数的。所以参见'ex1-46-r.scm'

(define (iterative-improve good-enough? improve)
    (lambda (guess)
        (let ((next (improve guess)))
            (if (good-enough? guess next)
                next
                ((iterative-improve good-enough? improve) (improve guess))))))

(define (sqrt x)
    (define tolerance 0.00001)
    (define (good-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (improve x)
        (lambda (guess)
            (average guess (/ x guess))))
    (define (average x y)
        (/ (+ x y) 2))
    ((iterative-improve good-enough? (improve x)) 1.0))

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (good-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    ((iterative-improve good-enough? f) first-guess))