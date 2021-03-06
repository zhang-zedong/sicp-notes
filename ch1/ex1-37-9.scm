(define (cont-frac n d k)
    (define (recur i)
        (if (> i k)
            0
            (/  (n i)
                (+  (d i)
                    (recur (+ i 1))))))
    (recur 1))

(define (1-of-phi k)
    (cont-frac  (lambda (i) 1.0)
                (lambda (i) 1.0)
                k))


(define (cont-frac-i n d k)
    (define (iter i res)
        (if (= i 0)
            res
            (let ((res (/ (n i) (+ (d i) res))))
                (iter (- i 1) res))
            ))
    (iter k 0))

(define (1-of-phi-i k)
    (cont-frac-i  (lambda (i) 1.0)
                (lambda (i) 1.0)
                k))


(define (e k)
    (+  2
        (cont-frac  (lambda (i) 1.0)
                    (lambda (i)
                        (let ((r (remainder i 3)))
                            (if (= r 2)
                                (* 2
                                   (+ 1 (/ (- i r) 3)))
                                1)))
                    k)))


; square 有重复计算， 可以加let消解
(define (tan-cf x k)
    (cont-frac  (lambda (i) 
                    (if (= i 1) (* 1.0 x) (- (square x))))
                (lambda (i)
                    (- (* 2 i) 1))
                k))