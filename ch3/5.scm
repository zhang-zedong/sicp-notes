(define (rand-update x) (random (expt 2 31)))
(define (random-init) (rand-update 0))
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-integral predicate x1 x2 y1 y2 trials)
  (abs 
    (* (monto-carlo trials predicate)
       (- x1 x2)
       (- y1 y2))))

(define (estimate-pi trials)
  (/ (estimate-integral predicate 2 8 4 10 trials) 9.0))

(define (predicate)
  (let ((x (random-in-range 2 8))
        (y (random-in-range 4 10)))
    (<= (+ (square (- x 5)) (square (- y 7))) 
        9)))

; don't change
(define (monto-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low 
       (* (/ (random (expt 2 31)) (expt 2 31))
          range)
       0.0)))