#lang racket

(define (compose f g)
  (lambda(x)
    (f (g x))))

(define (repeated f n)
  (if(= n 1)
     (lambda(x)
       (f x))
     (compose f (repeated f (- n 1)))))

;平均阻尼
(define (aver-damp f)
  (lambda(x)(/ (+ x (f x)) 2)))

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

;阻尼次数
(define (adump-times x)
  (if(= x 1)
     1
     (floor (/ (log x)(log 2)))))

;
(define (n-power-root x n)
  (let((times (adump-times n)))
    (fix-point (repeated
                (aver-damp (lambda(y)(/ x (expt y (- n 1)))))
                (adump-times n))
               1.0)))














































