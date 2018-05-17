#lang racket

;求平均数
(define (average x y)(/ (+ x y) 2))

;平均阻尼
(define (average-dump f)
  (lambda(x)(average x (f x))))

(define dx 0.00001)
(define (derive f)
  (lambda(x)(/ (- (f (+ x dx)) (f x)) dx)))


(define (cube x)(* x x x))
(define (square x)(* x x))



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

(define (sqrt x)
  (newton-method (lambda(y)(- (square y) x)) 1.0))





