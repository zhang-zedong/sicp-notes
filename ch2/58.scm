; sec 2.3.2
; plus exercise
; 还没写出来
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
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
          (error "unknown expression type: DERIV" exp))))
; exponentiation
(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base s) (car s))
(define (exponent s) (caddr s))
(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        (else (list base '** exponent))))
; variable and equal? base case
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
; sum
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (cddr s)))
; product
(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p)
  (if ((= length p) 3)
      (car p)
      (car p)))
(define (multiplicand p)
  (cond
    (((= length p) 3)
     (car p))
    ((eq? (cadddr p) '+)
     ())
    ((eq? (cadddr p) '*)
     ())
    (else (error "No type" p))))

; maker with combination for special case
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (append a1 '(+) a2))))
(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (append m1 '(*) m2))))
; test
; 遇到这个问题，除非改perdicates，所以遇到乘号，可能是加号，然后只要里面有加号，就是加号。这个就是计算最外层的。
; 找到所有的加法，然后中间的全加上括号，即找到最外层的所有加法，然后中间都是括号，分别运算，只有遇到全乘法，才是乘法，否则，就是加法
; 因为加法优先级低，先解析出来，参数自然是优先级高的了。


; A:
; (deriv '(x * (x * x)) 'x)
; (deriv '((x * x) * x) 'x)
; (deriv '(x + (x + x)) 'x)
; (deriv '((x + x) + x) 'x)
; B:
; (deriv '(x + (3 * (x + (y + 2)))) 'x)
; (deriv '(x * (x + (x * x))) 'x)
; (deriv '(x * (x * x)) 'x)