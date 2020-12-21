(define (double x)
    (+ x x))

(define (halve x)
    (if (= (remainder x 2) 0)
        (/ x 2)
        (/ (- x 1) 2)))

(define (even? x)
    (= (double (halve x)) x))

(define (*-expt a b)
    (cond ((= b 0) 0)
          ((even? b) (*-expt (double a) (halve b)))
          (else (+ a (*-expt a (+ b -1))))))