(define (accumulate-r combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner
            (term a)
            (accumulate-r combiner null-value term (next a) next b))))

(define (sum-r term a next b)
    (accumulate-r + 0 term a next b))

(define (product-r term a next b)
    (accumulate-r * 1 term a next b))

; 别人的有个写法，里面再写个iter函数，不动这个null-value，确实更合适一些。
(define (accumulate-i combiner null-value term a next b)
    (if (> a b)
        null-value
        (accumulate-i 
            combiner
            (combiner null-value (term a))
            term
            (next a)
            next
            b)))

(define (sum-i term a next b)
    (accumulate-i + 0 term a next b))

(define (product-i term a next b)
    (accumulate-i * 1 term a next b))

(define (inc i) 
    (+ i 1))
(define (identity i)
    i)
