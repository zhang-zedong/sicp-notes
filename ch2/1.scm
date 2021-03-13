(define (make-rat n d)
    (let ((g (gcd (abs n) (abs d))))
        (let ((nn (/ n g))
              (nd (/ d g)))
              (if (< d 0)
                (cons (- nn) (- nd))
                (cons nn nd)))))
          
        
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))