(define (queens board-size)
  (define (queen-cols k)
    (define empty-board ())
    (define (safe? k positions) ; last of positions is safe to rest of position
      (accumulate
        (lambda (x y) (and (not-check? (car positions) x) y))
        #t
        (cdr positions)))
    (define (not-check? a b)
      (and (not (= (cdr a) (cdr b)))
           (not (= (abs (- (car a) (car b)))
                   (abs (- (cdr a) (cdr b)))))))
    (define (adjoin-positions new-row k rest-of-queens) ; 倒着输出位置了
      (cons (cons k new-row) rest-of-queens))
    
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                      (adjoin-positions
                        new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op initial (cdr seq)))))

(define (enumerate-interval m n)
  (if (> m n)
      ()
      (cons m (enumerate-interval (+ m 1) n))))


(define (queens-solution-amount board-size)
  (accumulate
    (lambda (x y) (+ 1 y))
    0
    (queens board-size)))

(define (n-queen-amounts n)
  (map queens-solution-amount (enumerate-interval 1 n)))