(define (cons a b)
    (*  (expt 2 a)
        (expt 3 b)))


(define (car product)
    (if (= (remainder product 2) 0)
        (+ 1 (car (/ product 2)))
        0))

(define (cdr product)
    (if (= (remainder product 3) 0)
        (+ 1 (cdr (/ product 3)))
        0))
