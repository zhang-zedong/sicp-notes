; section 2.2.3

(define (enumerate-interval m n)
  (if (> m n)
      ()
      (cons m (enumerate-interval (+ m 1) n))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (test n)
  (accumulate 
    append () (map (lambda (i)
                      (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                  (enumerate-interval 1 n))))

; the difference between test and test-1 shows that flat is an adjetive, flat map a sequence of sequence.
(define (test-1 n)
  (map (lambda (i)
            (map (lambda (j) (list i j))
                (enumerate-interval 1 (- i 1))))
        (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

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
       (filter prime-sum? (flatmap
                            (lambda (i)
                              (map (lambda (j) (list i j))
                                   (enumerate-interval 1 (- i 1))))
                            (enumerate-interval 1 n)))))


(define (permutations s)
  (if (null? s)
      (list ())
      (flatmap (lambda (x)
                  (map (lambda (p) (cons x p))
                       (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))