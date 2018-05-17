#lang racket

(define (compose f g)
  (lambda(x)
    (f (g x))))

(define (repeated f n)
  (if(= n 1)
     (lambda(x)
       (f x))
     (compose f (repeated f (- n 1)))))


(define dx 0.00001)
(define (derive f)
  (lambda(x)(/ (- (f (+ x dx)) (f x)) dx)))

(define (avergae a b c)
  (/ (+ a b c) 3.0))

;平滑函数
(define (smooth f)
  (lambda(x)(avergae (f x) (f (- x dx)) (f (+ x dx)))))

(define (repeated-smooth f n)
  ((repeated smooth n) f))

(define (square x)(* x x))

















