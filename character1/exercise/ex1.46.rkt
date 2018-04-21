#lang racket

(define (iterative-improve good? improve)
  (lambda(x)
    (if(good? x)
       x
       (let ((guess (improve x)))
         ((iterative-improve good? improve) guess)))))

;平均阻尼
(define (average-dump f)
  (lambda(x)(average x (f x))))

;求平方
(define (square x)(* x x))

;求平均数
(define (average x y)(/ (+ x y) 2))

(define (good-enough? x f)
  (lambda(guess)(close-enough? (f guess) x)))

(define (sqrt x)
  ((iterative-improve (good-enough? x square)
                      (average-dump (lambda(guess)
                                      (/ x guess))))
   1.0))


;求不动点
(define tolerance 0.000001)
(define (close-enough? a b)
    (< (abs (- a b)) tolerance))
#|(define (fix-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if(close-enough? guess next)
         next
         (try next))))
  (try first-guess))|#

;自己写的
(define (fixed-good? f)
  (lambda(x)(close-enough? (f x) x)))
(define (fix-point f first-guess)
  ((iterative-improve (fixed-good? f) f)
   first-guess))



(define (sq x)
  (fix-point (average-dump (lambda(a)(/ x a))) 1.0))



#|
;参考他人答案
(define (iterative-improve good? improve)
  (lambda(first-guess)
    (define (try guess)
      (let((next (improve guess)))
        (if(good? next guess)
           next
           (try next))))
    (try first-guess)))


(define (fixed-point f first-guess)
  ((iterative-improve close-enough? f) first-guess))

(define (sqrt x)
  (fixed-point (average-dump(lambda(y)(/ x y)))
               1.0))|#












