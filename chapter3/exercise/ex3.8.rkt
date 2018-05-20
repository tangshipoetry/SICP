#lang racket

(define (f x)
  (set! f (lambda(y) 0))
  x)


(+ (f 1) (f 0))











































