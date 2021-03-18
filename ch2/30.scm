(define (square-tree x)
  (cond ((null? x) ())
        ((not (pair? x)) (square x))
        (else (cons (square-tree (car x)) (square-tree (cdr x))))))
; 最后一行应该是cons，把那个car x当成一个item就知道怎么弄。要么就是append list1 list2了

(define (square-tree-1 x)
  (if (pair? x)
      (map square-tree-1 x)
      (square x)))

(define (square-tree-2 tree)
  (map (lambda (subtree)
        (if (pair? subtree)
            (square-tree-2 subtree)
            (square subtree)))
       tree))


; test
(define x
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
