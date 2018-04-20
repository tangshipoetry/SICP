#lang racket

(define (aux i)
  (/ (* 2 (+ i 1)) 3))

(define (d k)
  (cond((= (remainder k 3) 2.0)(aux k))
       (else 1.0)))

(define n(lambda(i) 1.0))

#|(define (cont-frac n d k)
  (define (cf i)
    (if(= k i)
       (d k)
       (/ (n i)
          (+ (d i)
             (cf (+ 1 i))))))
  (cf 1))|#


(define (cont-frac n d k)
  (define (iter i result)
    (if(< i 1)
       result
       (iter (- i 1)
             (/ (n i)
                (+ (d i) result)))))
  (iter (- k 1) (/ (n k) (d k))))


























