;; 2.1.4 Extended Exercise: Interval Arithmetic
;; Working

; constructor and selectors
(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

; ex 2.8
(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let 
    ((p1 (* (lower-bound x) (lower-bound y)))
     (p2 (* (upper-bound x) (upper-bound y)))
     (p3 (* (lower-bound x) (upper-bound y)))
     (p4 (* (upper-bound x) (lower-bound y))))
    (make-interval 
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
    (error "Spans zero: DIV-INTERVAL Y" y)
    (mul-interval
      x
      (make-interval
        (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y))))))

; 默认了不为[0,0]，如果需要修改，所有小于号添加等号即可。
(define (mul-interval x y)
  (let
    ((a (lower-bound x))
     (b (upper-bound x))
     (c (lower-bound y))
     (d (upper-bound y)))
    (cond 
      ((and (>= a 0) (>= c 0)) (make-interval (* a c) (* b d)))
      ((and (>= a 0) (< d 0)) (make-interval (* b c) (* a d)))
      ((and (>= a 0) (< c 0) (>= d 0)) (make-interval (* b c) (* b d)))
      ((and (< b 0) (< d 0)) (make-interval (* b d) (* a c)))
      ((and (< b 0) (< c 0) (>= d 0)) (make-interval (* a d) (* a c)))
      ((and (< b 0) (>= c 0)) (make-interval (* a d) (* b c)))
      ((and (< a 0) (>= b 0) (< d 0)) (make-interval (* b c) (* a c)))
      ((and (< a 0) (>= b 0) (>= c 0)) (make-interval (* a d) (* b d)))
      ((and (< a 0) (>= b 0) (< c 0) (>= d 0))
       (make-interval (min (* a d) (* b c)) (max (* a c) (* b d)))))))
; 别人的答案吧区间判断抽象成函数, 如 positive-interval? negative-interval? spans-zero?

; case for 11
(define a (make-interval 1 2))
(define b (make-interval -2 -1))
(define c (make-interval -1 2))
; (mul-interval a b)

;; 12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; 考虑百分比的正负，解决c的正负号， (r以小数形式输入，不，r的输入形式不重要，装饰是另外的事儿）
(define (make-center-percent c p)
  (let ((w (abs (* c p))))
    (make-interval (- c w) (+ c w))))
; (make-center-percent 6.8 0.1)
; (make-center-percent 4.7 0.05)

(define (percent i)
  (/ (width i) (center i)))
; (percent (make-center-percent 6.8 0.1))


;; 13
(define (par1 r1 r2)
  (div-interval 
    (mul-interval r1 r2)
    (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one (add-interval (div-interval one r1) (div-interval one r2)))))

; (par1 (make-center-percent 6.8 0.1) (make-center-percent 4.7 0.05))
; (par2 (make-center-percent 6.8 0.1) (make-center-percent 4.7 0.05))


;; 14
; (define A (make-center-percent 6.8 0.1))
; (define B (make-center-percent 4.7 0.05))
; (center (div-interval A A))
; (percent (div-interval A A))
; (center (div-interval A B))
; (percent (div-interval A B))