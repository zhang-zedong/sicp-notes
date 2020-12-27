; Simpson's rule to calculate integration.

(define (simpson-integrate f a b n)
    (define h
        (/ (- b a) n))
    (define (inc i)
        (+ i 1))
    (define (term i)
        (define g
            (f (+ a (* i h))))
        (cond
            ((or (= i 0) (= i n))  ;再检查好几遍，发现这个地方应该是n，之前写成b了。
                g)
            ((even? i)
                (* 2 g))
            (else
                (* 4 g))))
    (*  (/ h 3)
        (sum term 0 inc n)))

(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (next a) next b))))

; (simpson-integrate cube 0 1 100)
; (simpson-integrate cube 0 1 1000)

; 这道题答案还是不对，不管h用分数还是小数，答案都是错一些。