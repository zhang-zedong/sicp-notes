(define (product term a next b)
    (if (> a b)
        1
        (* 
            (term a)
            (product term (next a) next b))))

(define (product-i term a next b)
    (define (iter i product)
        (if (> i b)
            product
            (iter (next i) (* (term i) product))))
    (iter a 1))

(define (inc i)
    (+ i 1))

(define (identity x) (x))

(define (factorial n)
    (product identity 1 inc n))

(define (calc-pi n)
    (define (pi-term i)
        (square
            (/
                (+ 2 (* 2 i))
                (+ 1 (* 2 i)))))
    (*
        8.0
        (product-i pi-term 1 inc n)
        (/ 1 (+ 1 (* 2 (+ n 1))))))