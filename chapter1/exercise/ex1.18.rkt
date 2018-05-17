#lang racket
(define (even? n)
  (= (remainder n 2) 0))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))


(define (f* a b)
  (cond ((= b 0) 0)
        ((even? b)(f* (double a) (halve b)))
        (else (+ a (f* a (- b 1))))))














