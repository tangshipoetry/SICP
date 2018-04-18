#lang racket

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

#|base的exp次幂对m取模|#
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square(expmod base
                                               (/ exp 2)
                                               m))
                                m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(define (fast-prime? n times)
  (cond((= times 0) #t)
       ((fermat-test n)(fast-prime? n (- times 1)))
       (else #f)))


(define (smallest-divison n)
  (find-divisor n 2))

(define (find-divisor n x)
  (cond((> (square x) n) n)
       ((divisor? n x) x)
       (else (find-divisor n (+ x 1)))))
(define (divisor? n x)
  (= 0 (remainder n x)))


;peimality test
(define (prime? n)
  (= n (smallest-divison n)))

;feemat-test
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))












