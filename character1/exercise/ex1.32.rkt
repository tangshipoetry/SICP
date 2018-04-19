#lang racket

;用accumulate实现product 和 sum
#|
;iterative
(define (accumulate combiner null-value term a next b)
  (define (accu-iter result a b)
    (if(> a b)
       result
       (accu-iter (combiner (term a) result)
                  (next a)
                  b)))
  (accu-iter null-value a b))
|#


(define (even? x)
  (= 0 (remainder x 2)))

(define (square x)
  (* x x))

;抽象accumulate过程
;recursive
(define (accumulate combiner null-value term a next b)
  (if(> a b)
     null-value
     (combiner (term a)
               (accumulate combiner null-value term (next a) next b))))

(define (factorial-item x)
  x)
(define (factorial-next x)
  (+ 1 x))
;用accumulate 构成product
(define (product term a next b)
  (accumulate * 1 term a next b))
;用accumulate构成sum
(define (sum term a next b)
  (accumulate + 0 term a next b))

;factorial测试
(define (factorial m)
  (product factorial-item 1 factorial-next m))

;计算圆周率测试
(define (pi-item x)
  (if(or (even? x)(= x 1))
     (error "paramater error")
     (/ (* (- x 1) (+ x 1)) (square x))))

(define (2+ x)
  (+ 2 x))

(define (pi-mul x)
  (* 4.0 (product pi-item 3 2+ x)))


















