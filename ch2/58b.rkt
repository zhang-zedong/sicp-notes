; sec 2.3.2
; current rule: 有+则+，有*则乘，有**则**。完全按照优先级来的any逻辑。
; 递归定义，符号优先级蕴含在判断顺序中
; 已完成基本功能，没有简化算式。同时没有使用memq等更简单逻辑，用的函数有些primitive，别人的答案琢磨多了，确实更精巧。
#lang sicp
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
          (deriv (base exp) var)))
        (else
          (error "unknown expression type: DERIV" exp))))
; exponentiation
; (deriv '(x ** x ** y) 'x)
; (deriv '(x ** x ** x) 'x)
(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base s) (car s))
(define (exponent s)
  (if (= (length s ) 3)
      (caddr s)
      (cddr s)))
(define (make-exponentiation base exponent)
  (cond ((=number? base 0) 0)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else (list base '** exponent))))
; variable and equal? base case
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))
; sum
; test
; (sum? '(x * x * x + x)) (addend '(x * x * x + x)) (augend '(x * x * x + x))
; (augend '(x * x + x + x + x)) (addend '(x * x + x + x + x))
; (augend '(x * x + (x + x + x))) (addend '(x * x + (x + x + x)))
; (deriv '((x * x) + x + (x * x)) 'x) (addend '((x * x) + x + (x * x))) (augend '((x * x) + x + (x * x)))
;; (deriv '((x + x) + x + x + (x + (x + (x + x)))) 'x)
; (sum? '(x * x * x))
(define (sum? x)
  (if (or (not (pair? x)) (= (length x) 1))
      #f
      (or (eq? (cadr x) '+) (sum? (cddr x)))))
(define (addend s)
  (define (recur s)
    (if (or (null? s) (eq? (car s) '+))
        nil
        (cons (car s) (recur (cdr s)))))
  (let ((ans (recur s)))
       (if (= (length ans) 1)
           (car ans)
           ans)))
(define (augend s)
  (if (eq? (car s) '+)
      (if (= (length (cdr s)) 1)
          (cadr s)
          (cdr s))
      (augend (cdr s))))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))
; product
; (product? '(x * x * x)) (product? '(x * x + x)) (product? '(x * x * x + x))
(define (product? x)
  (if (or (not (pair? x)) (= (length x) 1))
      #f
      (or (eq? (cadr x) '*) (product? (cddr x)))))
; (deriv '((x + x) * x * x + (x + (x * (x + x)))) 'x)
; '(x * x * x) '(x * x * x * x) '((x * x) * x * (x * (x + x)))
; test place
; (multiplicand '(x * x * x))
; (multiplicand '(x * x * x * x))
; (multiplicand '(x * x ** x))
; (multiplicand '(x ** x * x))
(define (multiplier p)
  (cond
    ((= (length p) 3) (car p))
    ((eq? (cadr p) '*) (car p))
    (else (append (list (car p) (cadr p)) (list (multiplier (cddr p)))))))
(define (multiplicand p)
  (cond
    ((= (length p) 3) (caddr p))
    ((eq? (cadr p) '*) (cddr p))
    (else (multiplicand (cddr p)))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; mix
; (deriv '(x * x * x + x * x) 'x)
; (deriv '((x + x) * (x ** 3)) 'x)
; (deriv '((x + x) * x ** 3) 'x)

