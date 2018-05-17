#lang racket
#|(define (factorial x)
  (if (= x 1)
      1
      (* x (factorial (- x 1)))))
|#

#|(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter n)
  (if (> counter n)
      product
      (fact-iter (* product counter)
                 (+ 1 counter)
                 n)))|#

(define (factorial n)
  (define (fact-iter product counter)
    (if (> counter n)
      product
      (fact-iter (* product counter)
                 (+ 1 counter))))
 (fact-iter 1 1))


























