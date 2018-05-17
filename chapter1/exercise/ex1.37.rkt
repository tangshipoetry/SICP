#lang racket
;无穷连分式

(define n(lambda(i) 1.0))
(define d(lambda(i) 1.0))

;recursive
#|(define (cont-frac n d k)
  (define (cf i)
    (if(= k i)
       (d k)
       (/ (n i)
          (+ (d i)
             (cf (+ 1 i))))))
  (cf 1))|#

;iterative
(define (cont-frac n d k)
  (define (iter i result)
    (if(< i 1)
       result
       (/ (n i)
          (+ (d i) result))))
  (iter (- k 1) (/ (n k) (d k))))


































