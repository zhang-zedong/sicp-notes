(define (make-monitored f)
  (let ((count 0))
    (lambda (in)
      (cond 
        ((eq? in 'how-many-calls?) count)
        ((eq? in 'reset-count)
          (begin (set! count 0) 0))
        (else (begin (set! count (+ count 1))
                    (f in)))))))
    

; this works now. Remember that procedure call need brackets.
(define (make-monitored-1 f)
  (let ((count 0))
    (define (how-many-calls?) count)
    (define (reset-count)
      (set! count 0)
      0)
    (define (inc1)
        (set! count (+ count 1)))
    (define (dispatch in)
      (cond 
        ((eq? in 'how-many-calls?) (how-many-calls?))
        ((eq? in 'reset-count) (reset-count))
        (else (begin (inc1) (f in)))))
    dispatch))
  

; 
; (define s (make-monitored sqrt))
; (s 100)
; (s 1)
; (s 'how-many-calls?)
; (s 'reset-count)
; (s 5)
; 
; 
; (define t (make-monitored-1 sqrt))
; (t 100)
; (t 1)
; (t 'how-many-calls?)
; (t 'reset-count)