#lang racket
;Fibonacci

;steps(time)指数增长，space线性增长
#|(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))|#

;time space liner increase
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b counter)
  (if(= counter 0)
     b
     (fib-iter (+ a b) a (- counter 1))))














