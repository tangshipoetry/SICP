#lang racket

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))



(define (mul-interval x y)
  (let((p1 (* (lower-bound x) (lower-bound y)))
       (p2 (* (upper-bound x) (upper-bound y)))
       (p3 (* (upper-bound x) (lower-bound y)))
       (p4 (* (lower-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
 
#|
修改div
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y)))))|#



(define (make-interval x y)
  (cons x y))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))




(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))



;被除区间跨过0
;不太懂为什么不能k跨过0
#|(define (div-interval x y)
  (let ((t (* (lower-bound y) (upper-bound y))))
    (if(= 0 y)
       (error "the divided interval contains zero")
       (if(< 0 t)
          (mul-interval x
                (make-interval (/ 1 (lower-bound y))
                               (/ 1 (upper-bound y))))
          (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y))))))))|#

(define (div-interval x y)
  (let ((t (* (lower-bound y) (upper-bound y))))
    (if(< 0 t)
       (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y))))
       (error "the divided interval contains zero"))))



(define (new-mul-interval x y)
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (if(> a 0)
       (if(> c 0)
          ()
          (if(< d 0)
             ()
             ()))
       (if(< b 0)
          (if(> c 0)
             ()
             (if(< d 0)
                ()
                ()))
          (if(> c 0)
             ()
             (if(< d 0)
                ()
                ()))))))



















