#lang racket

(define tolerance 0.000001)

(define (close-enough a b)
    (< (abs (- a b)) tolerance))

#|(define (fix-point f x)
  (cond((close-enough (f x) x) (f x))
       (fix-point f (f x))))|#



(define (fix-point f first-guess)
  )































