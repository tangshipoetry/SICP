#lang racket
;绝对值
(define (abs x)
  (if (< x 0) (- x) x))
;求平方值
(define (square x)(* x x))
;求平方根
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
;改进方法
(define (improve guess x)
  (average (/ x guess) guess))
(define (average x y)(/ (+ x y) 2.0))
;判断当前值是否足够好
(define (good-enough? guess x)
 (< (abs (- (square guess) x)) 0.0000001))