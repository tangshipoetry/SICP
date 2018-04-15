#lang racket
(define (square x)
  (* x x))
(define (even? n)
  (= (remainder n 2) 0))
;recurse
#|(define (fast-expt b n)
  (cond((= n 0) 1)
       ((even? n)(square (fast-expt b (/ n 2))))
       (else (* b (fast-expt b (- n 1))))))|#

;iterative
;我自己的
#|(define (fast-expt b n)
  (fast-expt-iter 1 b n))
(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (* a
                      (square (fast-expt-iter 1
                                              b
                                              (/ n 2)))))
        (else (fast-expt-iter (* a b)
                              b
                              (- n 1)))))|#
;参考答案
(define (fast-expt b n)
  (fast-expt-iter 1 b n))
(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a
                                   (square b)
                                   (/ n 2)))
        (else (fast-expt-iter (* a b)
                              b
                              (- n 1)))))




















