#lang racket

(define (cube x)(* x x x))
(define (square x)(* x x))

;导数
(define dx 0.00001)
(define (derive f)
  (lambda(x)(/ (- (f (+ x dx)) (f x)) dx)))

;寻找不动点
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

;牛顿变换以及牛顿法
(define (newton-transform g)
  (lambda(x)(- x (/ (g x) ((derive g) x)))))
(define (newton-method g guess)
  (fix-point (newton-transform g) guess))


(define (cubic a b c)
  (lambda(x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(define (cube-root a b c)
  (newton-method (cubic a b c) 1))





























