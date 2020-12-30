(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

(define (prime? n)
    (= n (smallest-divisor n)))

(define (smallest-divisor n)
    (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next n)
    (if (= n 2)
        3
        (+ n 2)))


(define (divides? a b)
    (= (remainder b a) 0))

; smallest prime larger than a number.
; 应该把count参数传递下去，或者把prime？函数包裹一下，包裹每次统计时间，为真输出，懒得改了，下次一定:)
(define (smallest-prime-from n count)
    (cond ((= count 0) "Complete")
          ((prime? n) 
            (timed-prime-test n)
            (smallest-prime-from (+ n 2) (- count 1)))
          (else (smallest-prime-from (+ n 2) count))))

; 从10^k+1开始的，三个质数
(define (start-from k)
    (define (exp k)
        (if (= k 0)
            1
            (* 10 (exp (- k 1)))))
    (define (calc k)
        (+ (exp k) 1))
    (smallest-prime-from (calc k) 3))