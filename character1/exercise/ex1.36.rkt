#lang racket

#||#

#|;平均阻尼
(define (fix-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if(close-enough? guess next)
         next
         (try (average guess next)))))
  (try first-guess))|#

#|
;自己写的
(define (fixed-point f first-guess)
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
    (try first-guess))|#

(define tolerance 0.000001)

(define (close-enough? a b)
    (< (abs (- a b)) tolerance))

(define (average x y)
  (/ (+ x y) 2))


;参考别人答案
(define (fixed-point f first-guess)
    (define (try guess step)
        (display-info guess step)                       ; 每次进入测试时打印一次猜测
        (let ((next (f guess)))
            (if (close-enough? next guess)
                (begin                                  ; 如果猜测完成
                    (display-info next (+ 1 step))      ; 记得算上最后一次计算 next 的猜测
                    next)
                (try next (+ 1 step)))))
    (try first-guess 1))

(define (display-info guess step)
    (display "Step: ")
    (display step)
    (display " ")
    
    (display "Guess: ")
    (display guess)
    (newline))

(define gold (lambda(x)
  (+ 1 (/ 1 x))))


(define (ex x)
  (expt x x))

(fixed-point (lambda(x)(/ (log 1000) (log x)))
           2.0)


(fixed-point (lambda(x)(average x (/ (log 1000) (log x))))
           2.0)

















