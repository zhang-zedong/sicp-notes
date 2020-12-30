(define (filtered-accumulate combiner null-value term a next b predicate?)
    (define (filter-exe i)
        (if (predicate? i) (term i) null-value))
    (if (> a b)
        null-value
        (combiner
            (filter-exe a)
            (filtered-accumulate combiner null-value term (next a) next b predicate?))))

; 学学函数起名字，可能就是学学英文语法吧
; sum-of-prime-squares
(define (f1 a b)
    (filtered-accumulate + 0 square a inc b prime?))

;product-of-relative-primes
(define (f2 n)
    (define (prime-n? i)
        (and (> i 0) (= (gcd m i) 1)))
    (filtered-accumulate * 1 identity 1 inc n prime-n?))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (inc i) (+ i 1))
(define (identity i) i)
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



