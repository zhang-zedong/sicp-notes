(define (square-list items)
  (if (null? items)
      ()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-1 items)
  (map square items))