#lang sicp

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (entry tree) (car tree))
    (define (left-branch tree) (cadr tree))
    (define (right-branch tree) (caddr tree))
    (define (make-tree entry left right)
      (list entry left right))
    (define (k entry) (car entry))
    (define (v entry) (cdr entry))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((= key (k (entry records))) (entry records))
            ((< key (k (entry records))) (assoc key (left-branch records)))
            (else (assoc key (right-branch records)))))
    (define (lookup key)
      (let ((record
             (assoc key (cdr local-table))))
        (if record
            (v record)
            #f)))
    (define (insert! key value)
      (define (insert tree)
        (cond ((null? tree) (make-tree (cons key value) '() '()))
              ((= key (k (entry tree)))
               (set-cdr! (entry tree) value)
               tree)
              ((< key (k (entry tree)))
               (make-tree (entry tree)
                          (insert (left-branch tree))
                          (right-branch tree)))
              (else
               (make-tree (entry tree)
                          (left-branch tree)
                          (insert (right-branch tree))))))
      (set-cdr! local-table
                (insert (cdr local-table)))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

; test
(define t (make-table))
((t 'insert!) 1 'a)
((t 'insert!) 2 'b)
((t 'insert!) 3 'c)
((t 'insert!) 4 'd)
((t 'insert!) 4 'f)
((t 'insert!) 5 'e)
((t 'lookup) 1)
((t 'lookup) 4)
