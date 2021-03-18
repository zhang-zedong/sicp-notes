(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; 因为逻辑上完备的，所有子集的集合，等于不包含此元素的，和包含此元素的。

;test
(define x (list 1 2 3))
