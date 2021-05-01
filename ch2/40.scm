(define (unique-pairs n)
  (accumulate 
    append 
    () 
    (map (lambda (i)
            (map (lambda (j) (list i j))
                 (enumerate-interval 1 (- i 1)))) 
         (enumerate-interval 1 n))))

(define (accumulate proc initial sequence)
  (if (null? sequence)
      initial
      (proc (car sequence)
            (accumulate proc initial (cdr sequence)))))

(define (enumerate-interval m n)
  (if (> m n)
      ()
      (cons m (enumerate-interval (+ m 1) n))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

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

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))