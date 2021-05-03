(define (equal? list1 list2)
  (cond 
    ((and (null? list1) (null? list2)) #t)
    ((or (null? list1) (null? list2)) #f)
    ((and (pair? (car list1)) (pair? (car list2)))
     (and (equal? (car list1) (car list2))
          (equal? (cdr list1) (cdr list2))))
    ((and (not (pair? (car list1))) (not (pair? (car list2))))
     (and (eq? (car list1) (car list2))
          (equal? (cdr list1) (cdr list2))))
    (else #f)))
; list? pair? symbol? all are implemented.

(display (equal? '(this is a list) '(this is a list)))
(display (equal? '(this is a list) '(this (is a) list)))
(display (equal? '(this (is a) list) '(this (is a) list)))
(display (equal? '(this (is a) list) '(this (is b) list)))