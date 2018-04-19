#lang racket

(define (sum term a next b)
  (if(> a b)
     0
     (+ (term a)
        (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if(> a b)
       result
       (iter (next a) (+ result (trem a)))))
  (iter a 0))

























