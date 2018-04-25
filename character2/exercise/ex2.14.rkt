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
  (let ((w (* c (/ p 100.0))))
    (make-center-width c w)))

(define (percent i)
  (/ (abs (/ (width i) (center i))) 100.0))


;——————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————————
(define (part1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (part2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))




(define a (make-center-percent 10 1))
(define b (make-center-percent 20 1))
(define c (part1 a b))
(define d (part2 a b))





















