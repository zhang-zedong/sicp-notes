(define (accumulate op default sequence)
  (if (null? sequence)
      default
      (op (car sequence)
          (accumulate op default (cdr sequence)))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (t)
                      (if (pair? t)
                          (count-leaves t)
                          1))
                   t)))

; map 函数里面的lambda，不出来null没问题，因为map里对null没法应用prc。

(define x (cons (list 1 2) (list 3 4)))
(define y (list (list 1 2 3 (list 4 5 (list 6 7)))))
; (count-leaves (list x x))