(define (miller-rabin-prime? n)
    (fermat-test? n 3)
    )

(define (fermat-test? n count)
    (cond ((= count 0) #t)
        ((= (expmod (random-in n) (- n 1) n) 1) (fermat-test? n (- count 1)))
        (else #f)))

(define (random-in n)
    (+ 1 (random (- n 1))))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
            (remainder (square (signal (expmod base (/ exp 2) m) m) ) m))
          (else (remainder (* base (expmod base (- exp 1) m)) m))))

; 把需要处理的函数，往外包裹一层函数（process），修改些返回值。
; 然后fermat-test？里面只用检测返回值是不是1，因为如果过程中出现signal，在这里就永远等于0
(define (signal t m)
    (cond ((or (= t 0) (= t 1) (= t (- m 1))) t)
          ((= (remainder (square t) m) 1) 0)
          (else t)))