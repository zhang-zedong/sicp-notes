(define (iterative-improve good-enough? improve)
    (lambda (guess)
            (if (good-enough? guess)
                guess
                ((iterative-improve good-enough? improve) (improve guess)))))

(define (sqrt x)
    (define tolerance 0.00001)
    (define (improve x)
        (lambda (g)
            (/ (+ g (/ x g))
               2)))

    ((iterative-improve (lambda (g) (< (abs (- (square g) x)) tolerance))
                        (improve x))
     1.0))

(define (fixed-point f first-guess)
    (define tolerance 0.00001)
    (define (good-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    ((iterative-improve (lambda (x) (good-enough? x (f x)))
                        f)
     first-guess))