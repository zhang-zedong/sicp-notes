; compute execute time
(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (fast-prime-t? n)
        (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))


; fast-prime
; 此函数包裹一下fast-prime，好更改计算次数。
(define (fast-prime-t? n)
    (fast-prime? n 3))

(define (fast-prime? n times)
    (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp) 
            (remainder 
                (square (expmod base (/ exp 2) m)) 
                m))
          (else 
            (remainder 
                (* base (expmod base (- exp 1) m)) 
                m))))


; entry function
; 从10^k+1开始的，三个质数
(define (start-from k)
    (define (exp k)
        (if (= k 0)
            1
            (* 10 (exp (- k 1)))))
    (define (calc k)
        (+ (exp k) 1))
    (smallest-prime-from (calc k) 3))

(define (smallest-prime-from n count)
    (cond ((= count 0) "Complete")
          ((fast-prime-t? n)
            (timed-prime-test n)
            (smallest-prime-from (+ n 2) (- count 1)))
          (else (smallest-prime-from (+ n 2) count))))