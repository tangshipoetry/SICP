#lang racket

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



;黄金分割率(/ (/ (+ a b) a) (/ b a))





















