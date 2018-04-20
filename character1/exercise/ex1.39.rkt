#lang racket

(define (square x)(* x x))

(define (cont-frac n d k)
  (define (iter i result)
    (if(< i 1)
       result
       (iter (- i 1)
             (/ (n i)
                (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))


(define (tan-cf x k)
  (define (d i)
    (- (* 2 i) 1))
  (define (n k)
    (cond((= k 1) x)
         (else (- (square x)))))
  (cont-frac n d k))






















