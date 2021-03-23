(define (accumulate op default sequence)
  (if (null? sequence)
      default
      (op (car sequence)
          (accumulate op default (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))
; 操作数只能应用于x，y还没有遍历到，不然每个y都应用了两遍。另外，最后的nil无法应用操作数

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; 所以 accumulate 设计的是一个二元操作符

; test
(map square (list 1 2 3))
(map (lambda (x) (* x 2)) (list 1 2 3))
(map square (list))

(append (list 1 2 3) (list 4 5 6))
(append () (list 1 2 3))
(append (list 1 2 3) ())
(append () ())

(length (list 1 2 3 4))
(length ())