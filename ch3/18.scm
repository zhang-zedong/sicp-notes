(define (cycle? li)
  (define visit '(head))
  (define (visited? x) (in? x (cdr visit)))
  (define (in? x li)
    (if (not (pair? li))
        #f
        (or (eq? (car li) x) (in? x (cdr li)))))
  (define (recur li)
    (cond
      ((not (pair? li)) #f)
      ((visited? li) #t)
      (else
        (append! visit (list li))
        (recur (cdr li)))))
  (recur li))

(define a '(a b c d))
(define b '(a b))
(define c '(d))
(set-cdr! (cdr b) c)
(set-cdr! c b)

;test
; (cycle? a)
; (cycle? b)