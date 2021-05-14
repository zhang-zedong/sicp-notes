; sum package
(define (install-sum-package)
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2))
           (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (sum exp var)
    ;(display exp) (newline)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '+ sum)
  (put 'make '+ make-sum)
  'done)
(define (install-product-package)
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (product exp var)
    ;(display exp) (newline)
    ((get 'make '+)
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag '* x))
  (put 'deriv '* product)
  (put 'make '* make-product)
  'done)


; exponentiation
(define (install-exponentiation-package)
  (define (base s) (car s))
  (define (exponent s) (cadr s))
  (define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list '** base exponent))))
  (define (exponentiation exp var)
    ((get 'make '*)
          ((get 'make '*) (exponent exp)
                        (make-exponentiation (base exp) ((get 'make '+) (exponent exp) -1)))
          (deriv (base exp) var)))
  (put 'deriv '** exponentiation)
  'done)
; deriv function
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
; basic function
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
; helper for tag system
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: CONTENTS" datum)))
; put and get
(define *op-table* (make-hash-table))
(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))
(define (get op type)
  (hash-table/get *op-table* (list op type) #f))
; install package
(install-sum-package)
(install-product-package)
(install-exponentiation-package)
;; test
;; for add system
; (deriv '(+ x x) 'x)
; (deriv '(* x x) 'x)
; (deriv '(* (+ x x) (* x x)) 'x)
; (deriv '(** x 3) 'x)
; (deriv '(** (+ x y) (+ x y)) 'x)