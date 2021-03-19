; structure = weight or mobile

; constructors
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; selectors
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length structure)
  (car structure))

; get structure, which is mobile or weight
(define (branch-structure structure)
  (cdr structure))

; usages
; 这里如果用if的话，会出问题，let的部分会提前加载一样。
; mobile 不会是一个数字，加这个也只是给下面的balanced打补丁
(define (total-weight mobile)
  (cond ((not (pair? mobile)) mobile) 
        (else
          (let ((left-branch-structure (branch-structure (left-branch mobile)))
                (right-branch-structure (branch-structure (right-branch mobile))))
            (+
              (if (pair? left-branch-structure)
                  (total-weight left-branch-structure)
                  left-branch-structure)
              (if (pair? right-branch-structure)
                  (total-weight right-branch-structure)
                  right-branch-structure))))))

; mobile 尾部有可能数字，这时候，函数没法往下用
(define (balanced? mobile)
  (cond 
    ((not (pair? mobile)) #t)
    ((= (* (branch-length (left-branch mobile))
           (total-weight (branch-structure (left-branch mobile))))
        (* (branch-length (right-branch mobile))
           (total-weight (branch-structure (right-branch mobile)))))
     (and  (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile)))))
    (else #f)))


; test
(define x 
  (make-mobile
    (make-branch 4 
                 (make-mobile 
                    (make-branch 2 2)
                    (make-branch 1 4)))
    (make-branch 6 4)))

(define y
  (make-mobile
    (make-branch 4 4)
    (make-branch 4
                 (make-mobile
                  (make-branch 2
                               (make-mobile
                                (make-branch 1 1)
                                (make-branch 1 1)))
                  (make-branch 2 2)))))

(define z
  (make-mobile
    (make-branch 1 1)
    (make-branch 1 1)))