(define (cycle? li)
  (define (recur a b)
    (cond 
      ((not (and (pair? b) (pair? (cdr b)))) #f)
      ((eq? a b) #t)
      (else (recur (cdr a) (cddr b)))))
  (if (not (and (pair? li) (pair? (cdr li))))
      #f
      (recur (cdr li) (cddr li))))


(define a '(a b c d e f))
(define b '(a b))
(define c '(d))
(set-cdr! (cdr b) c)
(set-cdr! c b)

;test
; (cycle? a)
; (cycle? b)