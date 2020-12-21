(define (double x)
    (+ x x))

(define (halve x)
    (if (= (remainder x 2) 0)
        (/ x 2)
        (/ (- x 1) 2)))

(define (even? x)
    (= (double (halve x)) x))

(define (*-expt a b)
    (cond ((= b 0) 0)
          ((even? b) (*-expt (double a) (halve b)))
          (else (+ a (*-expt a (+ b -1))))))

; 上面倒数第二行这行写的不够正宗 (一半没递归似的，他们得最后才能算出来答案)
; (double (*-expt a (halve b)))

; 最终，要么用even，要么用remainder， 所以还不如别人的答案里，用even

(define (*-x a b)
    (define (double x) (+ x x))
    (define (halve x) (/ x 2))
    (define (recur a b)
        (cond ((= b 0) 0)
            ((even? b) (double (recur a (halve b))))
            (else (+ a (recur a (- b 1))))))
    (recur a b)
)