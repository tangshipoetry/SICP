#lang racket

(define (average x y)
  (/ (+ x y) 2.0))

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



;打印点
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))





































