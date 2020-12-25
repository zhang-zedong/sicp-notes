; Simpson's rule to calculate integration.

(define (simpson-integrate f a b n)
    (define (h)
        (* 1.0 (/ (- b a) n)))
    (define (next i)
        (+ i 1))
    (define (term k)
        (f (+ a (* k (h)))))
    (define (simpson-sum term i next j)
        (cond
            ((> i j) 0)
            ((or (= i 0) (= i j))
                (+ (term i)
                   (simpson-sum term (next i) next j)))
            ((even? i)
                (+ (* 2 (term i))
                   (simpson-sum term (next i) next j)))
            (else
                (+ (* 4 (term i))
                   (simpson-sum term (next i) next j)))))

    (*  (/ (h) 3)
        (simpson-sum term 0 next n)))

;(simpson-integrate cube 0 1 100)
;(simpson-integrate cube 0 1 1000)
