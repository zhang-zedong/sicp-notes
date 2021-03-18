; structure = weight or mobile

; constructors
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; selectors
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length structure)
  (car structure))

(define (branch-structure structure)
  (car (cdr structure)))

; usages
(define (total-weight mobile)
  (+
    (if (pair? (branch-structure (left-branch mobile)))
        (total-weight (left-branch mobile))
        (branch-structure (left-branch mobile))) 
    (if (pair? (branch-structure (right-branch mobile)))
        (total-weight (right-branch mobile))
        (branch-structure (right-branch mobile)))))




(define (balanced? mobile)
  (if (= (+ (branch-length (left-branch mobile))
            (total-weight (branch-structure (left-branch mobile))))
         (+ (branch-length (right-branch mobile))
            (total-weight (branch-structure (right-branch mobile)))))
      (and (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))
      #f))

(define (balanced-1? mobile)
  (cond ((not (balanced-1? (branch-structure (left-branch mobile)))) #f)
        ((not (balanced-1? (branch-structure (right-branch mobile)))) #f)
        (= (+ (branch-length (left-branch mobile))
              (total-weight (branch-structure (left-branch mobile))))
           (+ (branch-length (right-branch mobile))
              (total-weight (branch-structure (right-branch mobile)))))))


; test
(define x 
  (make-mobile
    (make-branch 4 
                 (make-mobile 
                    (make-branch 2 2)
                    (make-branch 1 4)))
    (make-branch 6 4)))