#lang sicp
;; deque

(define (node item)
  (cons item (cons '() '())))

(define (connect-node! n1 n2)
  (set-cdr! (cdr n1) n2)
  (set-car! (cdr n2) n1))

; constructor
(define (make-deque) (cons '() '()))

; predicte
(define (empty-deque? deque)
  (null? (front-ptr deque)))

; selectors
; front-deque rear-deque
(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (car (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (car (rear-ptr deque))))

; mutators

(define (set-front-ptr! deque node)
  (set-car! deque node))

(define (set-rear-ptr! deque node)
  (set-cdr! deque node))

(define (front-insert-deque! deque item)
  (let ((new-node (node item)))
    (cond ((empty-deque? deque)
            (set-front-ptr! deque new-node)
            (set-rear-ptr! deque new-node)
            deque)
          (else
            (connect-node! new-node (front-ptr deque))
            (set-front-ptr! deque new-node)
            deque))))

(define (rear-insert-deque! deque item)
  (let ((new-node (node item)))
    (cond ((empty-deque? deque)
            (set-front-ptr! deque new-node)
            (set-rear-ptr! deque new-node)
            deque)
          (else
            (connect-node! (rear-ptr deque) new-node)
            (set-rear-ptr! deque new-node)
            deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
          (error "DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
          (set-front-ptr! deque '())
          (set-rear-ptr! deque '())
          deque)
        (else
          (let ((new-head (cddr (front-ptr deque))))
            (set-car! (cdr new-head) '())
            (set-front-ptr! deque new-head)
            deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
          (error "DELETE! called with an empty deque" deque))
        ((eq? (front-ptr deque) (rear-ptr deque))
          (set-front-ptr! deque '())
          (set-rear-ptr! deque '())
          deque)
        (else
          (let ((new-rear (cadr (rear-ptr deque))))
            (set-cdr! (cdr new-rear) '())
            (set-rear-ptr! deque new-rear)
            deque))))

(define (print deque)
  (define (iter cur-ptr)
    (if (null? cur-ptr)
        (newline)
        (begin
          (display (car cur-ptr))
          (display " ")
          (iter (cddr cur-ptr)))))
  (iter (car deque)))
  

;; test
; (define dq (make-deque))
; (front-insert-deque! dq 1)
; (front-insert-deque! dq 2)
; (front-insert-deque! dq 3)
; (front-insert-deque! dq 4)
