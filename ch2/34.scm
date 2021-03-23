(define (accumulate op default sequence)
  (if (null? sequence)
      default
      (op (car sequence)
          (accumulate op default (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;The object 0 is not applicable.
; higher-terms 本身包含运算符，不能再加括号，而且这个括号，是函数的意思。higher-terms就是算出来的一个值。

;Unbound variable: horner-veal
; 第二天，这个bug莫名消失，代码对了，猜是之前上下文污染了。

; 虽是填写accumulate参数，但是default的值就像是填写 递归 空值的 返回值，这些写法就像是换了个形式。

; test
; (horner-eval 2 (list 1 3 0 5 0 1))
; (horner-eval 2 ())
; (horner-veal 2 (list 1))