(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
; mit-scheme map 没有这个扩展版本

(define (matrix-*-vector m v)
  (map (lambda (m-row) (dot-product v m-row)) m))

(define (transpose mat)
  (accumulate-n cons () mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
       (map (lambda (m-vector) (matrix-*-vector cols m-vector)) m)))

;test
(dot-product (list 1 2 3) (list 4 5 6))