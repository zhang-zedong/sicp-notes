; 想了好久做出来了这一道题，优化前单独备份一遍。不用git，就是为了放在主目录。
; sec 2.3.2
; plus exercise
; 全是*才是*，有+则+
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
; exp 里面 由于有 (- exp 1) 导致这个地方不是数字会报错, 改为(make-sum exp -1)
(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))
(define (base s) (car s))
(define (exponent s) (caddr s))
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
; (deriv '((x + x) + x + x + (x + (x + (x + x)))) 'x)
; (sum? '(x * x * x))
(define (sum? x)
  (if (or (not (pair? x)) (= (length x) 1))
      #f
      (or (and (pair? x) (eq? (cadr x) '+))
          (sum? (cddr x)))))
(define (addend s)
  (define (recur s)
    (if (or (null? s) (eq? (car s) '+))
        ()
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
  (define (recur? x)
    ;(begin (display x) (newline)
      (cond ((not (pair? x)) #f)
            ((= (length x) 1) #t)
            (else (and (eq? (cadr x) '*) (recur? (cddr x))))))
  (cond ((not (pair? x)) #f)
        ((= (length x) 1) #f)
        (else (recur? x))))
; (deriv '((x + x) * x * x + (x + (x * (x + x)))) 'x)
; '(x * x * x) '(x * x * x * x) '((x * x) * x * (x * (x + x)))
; test place
(define t1 '(x * x * x))
(define t2 '(x * x * x * x))
(define t3 '((x * x) * x * (x * (x + x))))
(define (multiplier p)
  (car p))
(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (cddr p)))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; maker with combination for special case

; 这个地方不能用append，把原有的括号结构给拍没了
; test
; 遇到这个问题，除非改perdicates，所以遇到乘号，可能是加号，然后只要里面有加号，就是加号。这个就是计算最外层的。
; 找到所有的加法，然后中间的全加上括号，即找到最外层的所有加法，然后中间都是括号，分别运算，只有遇到全乘法，才是乘法，否则，就是加法
; 因为加法优先级低，先解析出来，参数自然是优先级高的了。

; 关于exp n只能是证书，
; A:
; (deriv '(x * (x * x)) 'x)
; (deriv '((x * x) * x) 'x)
; (deriv '(x + (x + x)) 'x)
; (deriv '((x + x) + x) 'x)
; (deriv '(x * (x + x)) 'x)
; (deriv '(x ** 10) 'x)
; (deriv '(x ** (5 * 5)) 'x)
; (deriv '(x ** (1 + y)) 'x)
; (deriv '(x ** x) 'x)
; (deriv '(x + (3 * (x ** (y + 2)))) 'x)
; B:
; (deriv '(x + x + x) 'x)
; (deriv '(x + (x + (x + x))) 'x)
; (deriv '(x + (x + (x * x))) 'x)

; (deriv '(x + (3 * (x + (y + 2)))) 'x)
; (deriv '(x * (x + (x * x))) 'x)
; (deriv '(x * (x * x)) 'x)