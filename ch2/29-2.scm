; 学习别人的
; 主要是后面复杂函数，没有抽象分层的意识。

; constructors
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; selectors
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length structure) (car structure))
(define (branch-structure structure) (cadr structure))

; mobile weight
(define (mobile? s) (pair? s))
(define (weight? s)
  (and (not (mobile? s)) (number? s)))

(define (mobile-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

; 这种复发的问法，即使简单，也应该抽象一下，因为人不用再去想一遍bool逻辑了，只是识别单词语义就好
(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (weight? structure)
        structure
        (mobile-weight structure))))
      
(define (mobile-balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (define (branch-balanced? branch)
    (let ((structure (branch-structure branch)))
      (or (weight? structure)
          (mobile-balanced? structure))))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (branch-balanced? left)
         (branch-balanced? right)
         (= (branch-torque (left-branch mobile))
            (branch-torque (right-branch mobile))))))


; 再看看这个total-weight，每行代码都需要读细节，还是给机器写的东西
; not pair mobile，很难读
; 然后两个let变量，像是获取structure抽象，然后下面有一段结构工整的代码
; 确实感觉很清楚，但是出错全错，并不想看，就是“感觉”工整。
; 反过来想，工整意味着有重复，有重复则应该抽象出来逻辑
; mine
; 两个函数就是嵌套的
(define (total-weight mobile)
  (define (branch-weight branch)
    (if (pair? branch)
        (total-weight branch)
        branch))
  (cond ((not (pair? mobile)) mobile) 
        (else
          (let ((left-branch-structure (branch-structure (left-branch mobile)))
                (right-branch-structure (branch-structure (right-branch mobile))))
            (+  (branch-weight left-branch-structure)
                (branch-weight right-branch-structure))))))

; mobile 尾部有可能数字，这时候，函数没法往下用
(define (balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch) (total-weight (branch-structure branch))))
  (define (weight? mobile) (not (pair? mobile)))
  (or (weight? mobile)
      (and (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))


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