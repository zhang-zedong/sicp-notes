(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (newline)
            (display next)
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))


(define (average a b)
    (/ (+ a b) 2))

(newline)
(display "with")
(define x-x-exp
    (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
                  2.0))
(newline)
(display "without")
(define x-x-exp-no-avg
     (fixed-point (lambda (x) (/ (log 1000) (log x)))
                  2.0))
