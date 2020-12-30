(define (carmichael n)
    (define (test n a)
        (cond ((= a 0) #t)
              ((= (expmod a n n) a) (test n (- a 1)))
              (else #f)
            ))

    (define (expmod base exp m)
        (cond ((= exp 0) 1)
            ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
            (else (remainder (* base (expmod base (- exp 1) m)) m))))


    (test n (- n 1))
)