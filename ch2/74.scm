; 这题我以为象征性写写文字就行了，别人还真能写出来函数？
; https://wizardbook.wordpress.com/2010/12/08/exercise-2-74/
;; a
(define (make-hq-file division file)
  (cons division file))
(define (file-division hq-file)
  (car hq-file))
(define (original-file hq-file)
  (cdr hq-file))

(define (get-record employee hq-file)
  ((get 'get-record (file-division hq-file))
   employee (original-file hq-file)))
(define (has-record? employee division)
  ((get 'has-record? division) employee))
; HQ：每个分区有自己的文件结构，HQ必须要创建自己的文件结构，包含分区名和原始分区文件
; DIV：每个分区必须实现两个函数，一个输入员工key，输出记录；另一个判断一个员工在不在此分区
;     这个函数实现后，需要由HQ安装/集成到他的程序里 (put 'get-record 'divi foo)
; （所以这里头是权责划分啊，HQ管集成，DIV自治文件，实现功能函数）

;; b
; HQ should create their own record containing the division name and the original record from the division
(define (make-hq-record division record)
  (cons division record))
(define (record-division hq-record)
  (car hq-record))
(define (original-record hq-record)
  (cdr hq-record))

(define (get-salary hq-file)
  ((get 'get-salary (file-division hq-file))
   (original-record hq-file)))

;; c
; HQ's own generic procedures are used to abstract away each division's own structures and procedures
(define (find-employee-record employee files)
  (cond ((null? files) (error "FIND-EMPLOYEE-RECORD : No such employee." employee))
        ((has-record? employee (file-division (car files)))
         (get-record employee (car files)))
        (else (find-employee-record employee (cdr files)))))

;; d
; Each new company must have its own unique division identifier,
; and use that to install their own specific versions of their own function.

(define (install-ultra-mega-corp)
  (put 'ultra-mega-corp 'get-record ultra-mega-corp-get-record)
  (put 'ultra-mega-corp 'has-record? ultra-mega-corp-has-record?)
  (put 'ultra-mega-corp 'get-salary ultra-mega-corp-get-salary))
