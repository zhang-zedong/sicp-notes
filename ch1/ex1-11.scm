; Exercise 1-11
; recursive process
(define (f n)
    (if (< n 3)
        n
        (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))) ))) 


; iterative process
(define (f-i n)
    (define (next a b c)
        (+ c (* 2 b) (* 3 a)))
    ;第i个数，指向c的位置
    (define (calc i a b c)
        (if (= n i)
            c
            (calc (+ i 1) b c (next a b c))))
    
    (if (< n 3)
        n
        (calc 2 0 1 2))
)

; iterative process
; 上面的写法还是感觉有点儿长，参照书上的"fib", "fib-iter"，看能不能化简.
(define (ff n)
    (define (ff-iter a b c count)
        (define (next a b c)
            (+ c (* 2 b) (* 3 a)))
        (if (= count 0)
            a
            (ff-iter b c (next a b c) (- count 1))))

    (ff-iter 0 1 2 n))

