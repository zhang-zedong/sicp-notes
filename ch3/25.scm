#lang sicp

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (assoc keys records)
      (cond ((null? records) false)
            ((equal? keys (caar records)) (car records))
            (else (assoc keys (cdr records)))))
    (define (lookup keys)
      (let ((record
             (assoc keys (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! keys value)
      (let ((record
             (assoc keys (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons keys value)
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

; test
(define t (make-table))
((t 'insert!) (list 'math '*) "mul")
((t 'insert!) (list 'math '+) "plus")
((t 'lookup) (list 'math '*))

((t 'insert!) (list 'math '*) "multiply")
((t 'lookup) (list 'math '*))

(define t-multi (make-table))
((t-multi 'insert!) (list 'a) "a")
((t-multi 'insert!) (list 'b) "b")
((t-multi 'insert!) (list 'a 'a) "aa")
((t-multi 'insert!) (list 'a 'b) "ab")
((t-multi 'insert!) (list 'a 'a 'a) "aaa")
((t-multi 'insert!) (list 'a 'a 'b) "aab")
((t-multi 'insert!) (list 'a 'a 'a 'a 'a) "aaaaa")

((t-multi 'lookup) (list 'a 'a 'a))
((t-multi 'lookup) (list 'a 'a 'a 'a 'a))
((t-multi 'lookup) (list 'b))