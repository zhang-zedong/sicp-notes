#lang sicp

; get / put
(#%require (only racket/base make-hash hash-ref hash-set!))
(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) #f))