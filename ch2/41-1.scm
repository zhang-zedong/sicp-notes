(define (unique-triples n)
  (flatmap (lambda (i)
              (flatmap (lambda (j)
                          (map (lambda (k)
                                  (list i j k))
                               (enumerate-interval 1 (- j 1))))
                       (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
; 这段牛逼，得再学下
; 反过来想，是把每一个函数应用于i，对每个函数来说，i是个固定的值，上层传过来的固定值
; 对k来说，i,j都是不可该的，而他的输出值是(list i j k)，一次函数运行的结果 

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (accumulate proc initial sequence)
  (if (null? sequence)
      initial
      (proc (car sequence) (accumulate proc initial (cdr sequence)))))

(define (make-triple-sum triple)
  (append triple
         (list (fold-right + 0 triple))))
; sum trick, accumulate
; feel like you need to think all function as a stream

(define (triple-sum-to-n n sum)
  (map make-triple-sum
       (filter (lambda (triple)
                  (= (sum-of-triple triple) sum))
               (unique-triples n))))

(define (sum-of-triple triple)
  (+ (car triple) (cadr triple) (cadr (cdr triple))))

(display (unique-triples 6))
(display (triple-sum-to-n 6 12))