#lang racket

(define (average a b)
  (/ (+ a b) 2.0))

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (close-enough? a b)
  (< (abs (- a b)) 0.0001))

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if(close-enough? neg-point pos-point)
       mid-point
       (let ((test-value (f mid-point)))
         (cond ((positive? test-value)
                (search f neg-point mid-point))
               ((negative? test-value)
                (search f mid-point pos-point))
               (else mid-point))))))


(define (half-check f a b)
  (let ((a-value (f a))(b-value (f b)))
    (cond((and (< a-value 0)(> b-value 0)) (search f a b))
         ((and (> a-value 0)(< b-value 0)) (search f b a))
         (else (error "error paramaters")))))




















