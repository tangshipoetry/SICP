#lang racket
(define (square x)(* x x))
(define (cube-root x)
  (curt-iter 1.0 x))
(define (curt-iter guess x)
  (let ((new-guess (improve guess x)))
    (if (good-enough? guess new-guess)
        new-guess
        (curt-iter new-guess x))))
(define (good-enough? old new)
  (> 0.000001 (abs (/ (- old new) old))))
(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))



