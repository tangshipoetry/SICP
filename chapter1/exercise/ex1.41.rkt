#lang racket

(define dx 0.00001)
(define (derive f)
  (lambda(x)(/ (- (f (+ x dx)) (f x)) dx)))

(define tolerance 0.000001)
(define (close-enough? a b)
    (< (abs (- a b)) tolerance))
(define (fix-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if(close-enough? guess next)
         next
         (try next))))
  (try first-guess)) 

(define (newton-transform g)
  (lambda(x)(- x (/ (g x) ((derive g) x)))))
(define (newton-method g guess)
  (fix-point (newton-transform g) guess))


(define (double f)
  (lambda(x)(f (f x))))

(define (inc x)(+ 1 x))











































