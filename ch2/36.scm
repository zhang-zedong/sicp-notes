(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (carcar seqs))
            (accumulate-n op init (cdrcar seqs)))))

(define (carcar seqs)
  (if (null? seqs)
      ()
      (cons (car (car seqs))
            (carcar (cdr seqs)))))

(define (cdrcar seqs)
  (if (null? seqs)
      ()
      (cons (cdr (car seqs))
            (cdrcar (cdr seqs)))))

(define (map proc seq)
  (if (null? seq)
      ()
      (cons (proc (car seq))
            (map proc (cdr seq)))))

(define (accumulate-n-1 op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n-1 op init (map cdr seqs)))))

(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))