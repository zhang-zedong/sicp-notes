(define (for-each proc items)
  (if (null? items)
      #t
      (begin
       (proc (car items))
       (for-each proc (cdr items)))))

(define (for-each-1 proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each-1 proc (cdr items)))))


; test
(for-each (lambda (x)
             (newline)
             (display x))
          (list 57 321 88))

(for-each-1 (lambda (x)
             (newline)
             (display x))
          (list 57 321 88))