#lang racket
;阿克曼函数
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))


;A函数，x为0，值为2*y ——>2*n
(define (f n)(A 0 n))
;A函数，x为1，y>1时，until y=1,multiplicate 2, recursive, when y = 1, value = 2, -------->>>2 to the power of n
(define (g n)(A 1 n))
;A函数，x 为2，2 to the power of (A 2 (- n 1))
;n =1-->2,n=2-->4, n=3--16,n=4-->2 to the power of 16m,65536,n=5-->2 to the power of 655636
(define (h n)(A 2 n))















