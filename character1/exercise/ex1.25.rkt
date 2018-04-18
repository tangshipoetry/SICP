#lang racket

#|(define (expmod base exp m)
  (cond ((= 1 exp) 1)
        ((even? exp) (square
                      (expmod
                       base
                       (/ exp 2) m)))
        (else
         (remainder (* base
                       (expmod base (- exp 1) m))
                    m))))|#

#|(define (expmod base exp m)
  (remainder (fast-expt base exp) m))|#


(define (even? x)
  (= 0 (remainder x 2)))

(define (square x)
  (* x x))

;按照书上脚注46所写，和书本上的有一些不同
(define (expmod base exp m)
  (cond ((= 0 exp) 1)
        ((even? exp) (remainder
                      (square(expmod base (/ exp 2) m))
                      m))
        (else
         (remainder (* (remainder base m)
                       (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond((= times 0) #t)
       ((fermat-test n)(fast-prime? n (- times 1)))
       (else #f)))


(define (fast-expt base exp)
  (cond((= 0 exp) 1)
       ((even? exp) (fast-expt (square base) (/ exp 2)))
       (else (* base (fast-expt base (- exp 1))))))


;理论上没有问题，但实际上不是很好，要完成大数运算，虽然可以快速进行，但是对于特大数字的运算，会导致实现overflow
#|(define (expmod base exp m)
    (remainder (fast-expt base exp) m))|#













