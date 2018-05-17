#lang racket

(define (square x)
  (* x x))

;iterative
(define (samllest-divisor n)
  (div-iter 2 n))

(define (div-iter x n)
  (cond((> (square x) n) n)
       ((= (remainder n x) 0) x)
       (else (div-iter (+ 1 x)
                       n))))
























