(define (g)
  (define y 0)
  (lambda (x)
      (if (= x 0)
          (begin
            (set! y 1)
            x)
          (- x y))))

(define f (g))


; ((g) 0)
; ((g) 1)
; (+ (f 0) (f 1))
; (+ (f 1) (f 0))