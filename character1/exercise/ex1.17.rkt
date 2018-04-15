#lang racket
(define (even? n)
  (= (remainder n 2) 0))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (fast* a b)
  (f-iter a b))


#|迭代方式|#
;无状态变量
(define (f-iter a b)
  (cond ((= 0 b) 0)
        ((even? b) (f-iter (double a) (halve b)))
        (else (+ a (f-iter a (- b 1))))))
;有状态变量
(define (f* a b)
  (fi 0 a b))
(define (fi p a b)
  (cond ((= 0 b) p)
        ((even? b) (fi p (double a) (halve b)))
        (else (fi (+ p a) a (- b 1)))))











