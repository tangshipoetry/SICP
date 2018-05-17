#lang racket

;recursive
#||#

#|
(define (product term a next b)
  (if(> a b)
     1
     (* (term a) (product term (next a) next b))))

(define (inc x)(+ 1 x))|#

(define (product term a next b)
  (define (mul-iter a result)
    (if(> a b)
       result
       (mul-iter (next a) (* result (term a)))))
  (mul-iter a 1))


(define (even? x)
  (= 0 (remainder x 2)))

(define (square x)
  (* x x))

(define (pi-item x)
  (if(or (even? x)(= x 1))
     (error "paramater error")
     (/ (* (- x 1) (+ x 1)) (square x))))

(define (2+ x)
  (+ 2 x))

(define (pi-mul x)
  (* 4.0 (product pi-item 3 2+ x)))

(define (factorial-item x)
  x)
(define (factorial-next x)
  (+ 1 x))
(define (factorial m)
  (product factorial-item 1 factorial-next m))





