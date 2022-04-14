(define (prime? n)
    ; miller-rabin-prime?
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
    (define (signal t m)
        (cond ((or (= t 0) (= t 1) (= t (- m 1))) t)
            ((= (remainder (square t) m) 1) 0)
            (else t)))
    (fermat-test? n 3))


; 不用 a>b 也行，第一次运算余数就反过来了。
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

; for MIT-scheme
; function table
; put and get borrowed
(define *op-table* (make-hash-table))
(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table/get *op-table* (list op type) #f))