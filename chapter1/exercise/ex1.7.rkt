#lang racket
#|
(define (square x)(* x x))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (good-enough? guess x)
        (< (abs (- (square guess) x)) 0.0000001))
(define (improve guess x)
  (average (/ x guess) guess))
(define (average x y)(/ (+ x y) 2))
|#
#|
(define (improve guess x)
  (average (/ x guess) guess))
(define (average x y)(/ (+ x y) 2))
(define (square x)(* x x))
(define (sqrt x)
  (sqrt-iter 0 1.0 x))
(define (sqrt-iter old guess x)
  (if (good-enough? old guess)
      guess
      (sqrt-iter guess (improve guess x) x)))
(define(good-enough? old new)
  (< (/ (abs (- old new)) new) 0.00001))
|#

(define (improve guess x)
  (average (/ x guess) guess))
(define (average x y)(/ (+ x y) 2))
(define (square x)(* x x))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (good-enough? old new)
  (> 0.000001 (abs (/ (- old new) old))))
(define (sqrt-iter guess x)
  (let ((new-guess (improve guess x)))
    (if (good-enough? guess new-guess)
        new-guess
        (sqrt-iter new-guess x))))





#|
(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      (improve guess x)
      (sqrt-iter (improve guess x) x)))
|#







