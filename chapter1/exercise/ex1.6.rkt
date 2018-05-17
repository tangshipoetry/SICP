#lang racket
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause) ))
;绝对值
(define (abs x)
  (if (< x 0) (- x) x))
;求平方值
(define (square x)(* x x))
;求平方根
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
;改进方法
(define (improve guess x)
  (average (/ x guess) guess))
(define (average x y)(/ (+ x y) 2.0))
;判断当前值是否足够好
(define (good-enough? guess x)
 (< (abs (- (square guess) x)) 0.0000001))
#|
栈溢出，if为特殊形式，两个表达式只会求一个，
new-if为一个普通过程，无论结果真假都会不断递归求值，最终导致内存不够
|#