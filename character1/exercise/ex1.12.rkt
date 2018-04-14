#lang racket
;pascal triangle
#|定位第m行，第n列 n(x,y)
计算：
n(x,y)=n(x-1,y-1)+n(x-1,y)
n(1,1)=1
n(x,x)=1
|#

(define (Pascal-element x y)
  (cond ((= x 1) 1)
        ((= y 1) 1)
        ((= x y) 1)
        (else (+ (Pascal-element (- x 1) y)
                 (Pascal-element (- x 1) (- y 1))))))





























