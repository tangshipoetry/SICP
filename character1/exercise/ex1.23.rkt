#lang racket
(define (square x)
  (* x x))

(define (even? x)
  (= 0 (remainder x 2)))

(define (samellest n)
  (fine-divisor 2 n))

(define (next-test x)
  (if(even? x)
     (+ 1 x)
     (+ 2 x)))

(define (divisor? n test)
  (= 0 (remainder n test)))

(define (fine-divisor  n test-divisor)
  (cond ((> (square test-divisor) n))
        ((divisor? n test-divisor) test-divisor)
        (else (fine-divisor  n (nest-test test-divisor)))))












