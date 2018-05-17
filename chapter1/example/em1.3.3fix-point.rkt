#lang racket

#|(define (fix-point f x)
  (cond((close-enough? (f x) x) (f x))
       (fix-point f (f x))))|#

(define tolerance 0.000001)

(define (close-enough? a b)
    (< (abs (- a b)) tolerance))

(define (average x y)
  (/ (+ x y) 2))





(define (fix-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if(close-enough? guess next)
         next
         (try next))))
  (try first-guess))



(define (sqrt x)
  (fix-point (lambda(y)(average y (/ x y)))
             1.0))

































