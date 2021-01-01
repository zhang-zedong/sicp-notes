(define (compose f g)
    (lambda (x)
        (f (g x))))

(define (inc i) (+ i 1))

; ((compose square inc) 6)


(define (repeated f n)
    (lambda (x)
        (if (= n 1)
            (f x)
            ((compose (repeated f (- n 1)) f) x))))

(define (repeated-1 f n)
    (lambda (x)
        (if (= n 0)
            x
            (compose f (repeated f (- n 1))))))

(define (repeated-2 f n)
    (if (< n 1)
        (lambda (x) x)
        (compose f (repeated f (- n 1)))))

(define (repeated-3 f n)
    (if (= n 1)
        f
        (compose f (repeated f (- n 1)))))

; ((repeated square 2) 5)


(define dx 0.00001)
(define (average a b c)
    (/ (+ a b c) 3))
(define (smooth f)
    (lambda (x)
        (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (n-fold-smooth f n)
    (lambda (x)
        (((repeated smooth n) f) x)))

(define (n-fold-smooth-1 f n)
    ((repeated smooth n) f))