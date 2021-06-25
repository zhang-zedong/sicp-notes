; in sicp, append is puting tow lists together.
(define (count-pairs x)
  (define visit '(head))
  (define (visited? x) (in? x (cdr visit)))
  (define (in? x li)
    (if (not (pair? li))
        #f
        (or (eq? (car li) x) (in? x (cdr li)))))
  (define (recur x)
    (if (or (not (pair? x)) (visited? x))
        0
        (begin
          (append! visit (list x))
          (+ (recur (car x))
              (recur (cdr x))
              1))))
  (recur x))

(define (count-pairs-o x)
  (if (not (pair? x))
      0
      (+ (count-pairs-o (car x))
         (count-pairs-o (cdr x))
         1)))
; test
(define s3 '(a b c))


(define s4 '(a c))
(define i1 '(b))
(set-car! s4 i1)
(set-car! i1 (cdr s4))

; (count-pairs s3)
; (count-pairs-o s3)

(define i2 '(c))
(define i3 '(a))
(set-car! i3 i2)
(set-cdr! i3 i2)
(define s7 '(a))
(set-car! s7 i3)
(set-cdr! s7 i3)

(define si '(a b))
(define i4 '(c))
(set-cdr! (cdr si) i4)
(set-cdr! i4 si)


