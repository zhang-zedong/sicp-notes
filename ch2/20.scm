;(define (same-parity . l)
;    (cond ((null? l) ())
;          ((null? (cdr l)) l)
;          (else (append (list (car l)) (same-parity (cdr (cdr l)))))))
          

(define (same-parity . l)
  (define (recur l)
    (cond ((null? l) ())
          ((null? (cdr l)) l)
          (else (append (list (car l)) (recur (cdr (cdr l)))))))
  (recur l))


(define (same-parity-1 x . l)
  (define (recur l)
    (cond ((null? l) ())
          ((null? (cdr l)) ())
          (else (cons (car (cdr l)) (recur (cdr (cdr l)))))))
  (cons x (recur l)))