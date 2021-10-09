#lang sicp

; leaf constructor and selector
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; tree constructor
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
; tree selector
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; decoder: bits string -> char string
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
; (make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;(decode sample-message sample-tree)

; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol symbol tree)
  (define (search branch)
    (cond
      ((leaf? branch)
       (if (equal? symbol (symbol-leaf branch))
           nil
           -1))
      ((memq symbol (symbols branch))
       (let ((left (search (left-branch branch))))
         (if (not (equal? left -1))
             (cons 0 left)
             (cons 1 (search (right-branch branch))))))
      (else -1)))
  (let ((bits (search tree)))
    (if (equal? bits -1)
        (error "non-vaild symbol: ENCODE-SYMBOL" symbol)
        bits)))
;(encode-symbol 'C sample-tree)

; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge set)
  (if (= (length set) 1)
      (car set)
      (let ((left (car set))
            (right (cadr set)))
        (let ((new-branch (make-code-tree left right)))
          (successive-merge (adjoin-set new-branch (cddr set)))))))

; (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
; (encode-symbol 'C (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
  
; 2.70
(define words-tree
  (generate-huffman-tree
   '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

(define words-message '(GET A JOB
                           SHA NA NA NA NA NA NA NA NA
                           GET A JOB
                           SHA NA NA NA NA NA NA NA NA
                           WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                           SHA BOOM))

; (length (encode words-message words-tree))

; 2.71
(define f-tree
  (generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16))))