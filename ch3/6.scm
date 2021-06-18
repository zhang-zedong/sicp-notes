(define (rand-update x) 
  (remainder (+ (* 97 x) 173) 137))

(define (rand-seed seed)
  (define num seed)
  (define (reset x)
    (set! num x)
    0)
  (lambda (command)
    (cond ((eq? command 'generate)
          (set! num (rand-update num))
          num)
          ((eq? command 'reset) reset))))
  

(define rand (rand-seed 0))
; (rand 'generate)
; ((rand 'reset) 0)