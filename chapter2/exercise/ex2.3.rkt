#lang racket

(define (average x y)
  (/ (+ x y) 2.0))
(define (square x)(* x x))
(define dx 0.00001)
(define (derive f)
  (lambda(x)(/ (- (f (+ x dx)) (f x)) dx)))
(define tolerance 0.000001)
(define (close-enough? a b)
    (< (abs (- a b)) tolerance))
(define (fix-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if(close-enough? guess next)
         next
         (try next))))
  (try first-guess))
(define (newton-transform g)
  (lambda(x)(- x (/ (g x) ((derive g) x)))))
(define (newton-method g guess)
  (fix-point (newton-transform g) guess))
(define (sqrt x)
  (newton-method (lambda(y)(- (square y) x)) 1.0))

;打印点
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;构造点
(define (make-point x y)
  (cons x y))
;点的x坐标
(define (x-point a)
  (car a))
;点的y坐标
(define (y-point a)
  (cdr a))

;构造线段
(define (make-segment start end)
  (cons start end))
;线段起点
(define (start-segment s)
  (car s))
;线段终点
(define (end-segment s)
  (cdr s))

;线段中点
(define (mid-point s)
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
              (average (y-point (start-segment s)) (y-point (end-segment s)))))

;求线段长度
(define (seg-len s)
  (sqrt (+ (square (- (x-point (start-segment s)) (x-point (end-segment s))))
           (square (- (y-point (start-segment s)) (y-point (end-segment s)))))))


#|


;构造长方形--对角线点表示
(define (make-rect start end)
  (cons start end))

;获取长
(define (get-x-seg rect)
  (let ((top-right (cdr rect))(lower-left (car rect)))
    (make-segment lower-left
                  (make-point (x-point top-right)
                              (y-point lower-left)))))
;获取宽
(define (get-y-seg rect)
  (let ((top-right (cdr rect))(lower-left (car rect)))
    (make-segment lower-left
                  (make-point (x-point lower-left)
                              (y-point top-right)))))
|#


;构造长方形--长款两条线段表示
(define (make-rect x-seg y-seg)
  (cons x-seg y-seg))
;获取长
(define (get-x-seg rect)
  (car rect))
;获取宽
(define (get-y-seg rect)
  (cdr rect))


;周长seg-len
(define (perimeter-rect r)
  (* 2 (+ (seg-len (get-x-seg r))
          (seg-len (get-y-seg r)))))

;面积
(define (area-rect r)
  (* (seg-len (get-x-seg r))
     (seg-len (get-y-seg r))))








