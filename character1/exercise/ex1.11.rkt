#lang racket
;recursive
#|
(define (f x)
  (cond ((< x 3) x)
        (else (+ (* (f (- x 3)) 3)
                 (* (f (- x 2)) 2)
                 (* (f (- x 1)) 1)))))
|#




;iterative
#||#

(define (f x)
  (f-iter 0 1 2 (- x 2)))
(define (f-iter a b c counter)
  (if(<= counter 0)
     c
     (f-iter b
             c
             (+ (* 3 a)
                (* 2 b)
                c)
             (- counter 1))))















