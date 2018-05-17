#lang racket

(define (make-interval x y)
  (cons x y))

(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

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

(define (div-interval x y)
  (let ((t (* (lower-bound y) (upper-bound y))))
    (if(< 0 t)
       (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y))))
       (error "the divided interval contains zero"))))


(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-center-width c w)))

(define (percent i)
  (/ (abs (/ (width i) (center i))) 100))








































