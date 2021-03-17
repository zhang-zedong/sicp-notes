(define (deep-reverse x)
  (cond ((null? x) ())
        ((not (pair? (car x)))
          (append (deep-reverse (cdr x)) (list (car x))))
        (else
          (append (deep-reverse (cdr x)) (list (deep-reverse (car x)))))))

; 最后一行需要加上list，(car x)是个item，那么需要包裹list。另一个角度看，car和list得配对

(define (deep-reverse-1 x)
  (if (pair? x)
      (reverse (map deep-reverse-1 x))
      x))

(define (deep-reverse-2 x)
  (cond ((null? x) ())
        ((not (pair? x)) x)
        (else
          (append (deep-reverse-2 (cdr x)) (list (deep-reverse-2 (car x)))))))


;test
(define x (list (list 1 2) (list 3 4)))
x
(deep-reverse x)

(define y (list (list 1 2) 3 (list 4 (list 5 (list 6 (list 7 8))))))
y
(deep-reverse y)

(deep-reverse-1 y)