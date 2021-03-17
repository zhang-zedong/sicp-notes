(define (fringe x)
  (cond ((null? x) ())
        ((not (pair? (car x)))
          (cons (car x) (fringe (cdr x))))
        (else
          (append (fringe (car x)) (fringe (cdr x))))))

(define (fringe-1 tree)
  (cond ((null? tree) ())
        ((not (pair? tree))
          (list tree))
        (else
          (append (fringe-1 (car tree)) (fringe-1 (cdr tree))))))


;test
(define x (list (list 1 2) (list 3 4)))
x
(fringe x)
(fringe (list x x))

(define y (list (list 1 2) 3 (list 4 (list 5 (list 6 (list 7 8))))))
y
(fringe y)