(define (make-segment start-point end-point)
    (cons start-point end-point))

(define (start-segment segment)
    (car segment))

(define (end-segment segment)
    (cdr segment))

(define (point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

(define (midpoint-segment segment)
    (define (average a b) (/ (+ a b) 2))
    (let ((p (point (average (x-point (start-segment segment)) (x-point (end-segment segment)))
                    (average (y-point (start-segment segment)) (y-point (end-segment segment))))))
        (print-point p)
        p))

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))

; (print-point (midpoint-segment (make-segment (point 1 3) (point 2 8))))
; (midpoint-segment (make-segment (point 1 3) (point 2 8)))

(define (length-seg segment)
    (sqrt (+
            (square (- (x-point (start-segment segment)) (x-point (end-segment segment))))
            (square (- (y-point (start-segment segment)) (y-point (end-segment segment)))))))

; (length-seg (make-segment (point 0 3) (point 4 0)))


; constructor
(define (make-rectangle left-segment bottom-segment)
    (cons left-segment bottom-segment))

; selectors
(define (left-segment rectangle)
    (car rectangle))

(define (bottom-segment rectangle)
    (cdr rectangle))

; utilities
(define (rect-height rectangle)
    (length-seg (left-segment rectangle)))

(define (rect-width rectangle)
    (length-seg (bottom-segment rectangle)))


; compute function
(define (peremeter rectangle)
    (* 2
       (+ (rect-height rectangle) (rect-width rectangle))))

(define (area rectangle)
    (* (rect-height rectangle) (rect-width rectangle)))

; test command:
;   (peremeter (make-rectangle (make-segment (point 0 0) (point 0 2)) (make-segment (point 0 0) (point 8 0))))
;   (area (make-rectangle (make-segment (point 0 0) (point 0 2)) (make-segment (point 0 0) (point 8 0))))


; constructor
(define (make-rectangle2 bottom-left top-right)
    (cons bottom-left top-right))

; selectors
(define (bottom-left rectangle) (car rectangle))

(define (top-right rectangle) (cdr rectangle))

; utilities
(define (rect-width2 rectangle)
    (abs (- (x-point (bottom-left rectangle)) (x-point (top-right rectangle)))))

(define (rect-height2 rectangle)
    (abs (- (y-point (bottom-left rectangle)) (y-point (top-right rectangle)))))

;test
;(rect-width2 (make-rectangle2 (point 0 0) (point 3 4)))
;(rect-height2 (make-rectangle2 (point 0 0) (point 3 4)))


; computer function
(define (peremeter2 rectangle)
    (* 2
       (+ (rect-width2 rectangle) (rect-height2 rectangle))))

(define (area2 rectangle)
    (* (rect-width2 rectangle) (rect-height2 rectangle)))


;(peremeter2 (make-rectangle2 (point 0 0) (point 3 4)))
;(area2 (make-rectangle2 (point 0 0) (point 3 4)))