(define (enumerate-interval m n)
  (if (> m n)
      ()
      (cons m (enumerate-interval (+ m 1) n))))

(define (ordered-triples n)
  (accumulate
    append
    ()
    (map (lambda (i)
            (map (lambda (j) (cons i j))
                 (ordered-pairs n)))
         (enumerate-interval 1 n))))

(define (ordered-triples-1 n)
  (accumulate
    append
    ()
    (map (lambda (i)
            (map (lambda (j) (cons i j))
                 (accumulate
                    append
                    ()
                    (map (lambda (j)
                            (map (lambda (k) (list j k))
                                 (enumerate-interval 1 n)))
                         (enumerate-interval 1 n)))))
         (enumerate-interval 1 n))))

(define (ordered-pairs n)
  (accumulate
    append
    ()
    (map (lambda (i)
            (map (lambda (j) (list i j))
                 (enumerate-interval 1 n)))
         (enumerate-interval 1 n))))

(define (accumulate proc initial sequence)
  (if (null? sequence)
      initial
      (proc (car sequence)
            (accumulate proc initial (cdr sequence)))))

; final answer function
(define (triples-sum-to n s)
  (filter (sum-equal? s) (ordered-triples n)))

(define (sum-equal? s)
  (lambda (triple)
    (= (sum-triple triple) s)))

(define (sum-triple triple)
  (+ (car triple)
     (cadr triple)
     (cadr (cdr triple))))
