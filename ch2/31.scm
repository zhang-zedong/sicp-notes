(define (tree-map proc x)
  (cond ((null? x) ())
        ((not (pair? x)) (proc x))
        (else (cons (tree-map proc (car x)) (tree-map proc (cdr x))))))

(define (tree-map-1 proc x)
  (map (lambda (subtree)
          (if (pair? subtree)
              (tree-map-1 proc subtree)
              (proc subtree)))
       x))

; bad answer
; 所以还是map概念好，然后把细节扔给lambda，像个信号工程师了
(define (tree-map-2 proc x)
  (if (pair? x)
      (map (lambda (x)
              (tree-map-2 proc x)) 
           x)
      (proc x)))

(define (square-tree tree) (tree-map square tree))

; test
(define x
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

(tree-map (lambda (x) (* x 3)) x)