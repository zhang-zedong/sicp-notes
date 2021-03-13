(define (last-pair li)
    (if (null? (cdr li))
        li
        (last-pair (cdr li))))

(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

(define (reverse li)
    (if (null? li)
        li
        (append (reverse (cdr li)) (list (car li)))))

(define (reverse-1 li)
    (if (null? (cdr li))
        li
        (append (reverse-1 (cdr li)) (list (car li)))))

(define (reverse-2 li)
    (define (iter items result)
        (if (null? items)
            result
            (iter (cdr items) (cons (car items) result))))
    (iter li ()))
