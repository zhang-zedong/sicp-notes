(define (cons a b)
    (*  (expt 2 a)
        (expt 3 b)))


(define (car product)
    (log-reduce product 2))

(define (cdr product)
    (log-reduce product 3))

(define (log-reduce n base)
    (if (= (remainder n base) 0)
        (+ 1 (log-reduce (/ n base) base))
        0))
