#lang racket


(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

#|base的exp次幂对m取模|#
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((nontrivial-root? base m) 0)
        ((even? exp) (remainder (square(expmod base
                                               (/ exp 2)
                                               m))
                                m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond((= times 0) #t)
       ((fermat-test n)(fast-prime? n (- times 1)))
       (else #f)))


;Miller-Rabin-test
(define (MR-test n)
  (define a (+ 1 (random (- n 1))))
  (= 1 (expmod a (- n 1) n)))

;非凡平方根检测
(define (nontrivial-root? a n)
  (and
   (not (= a 1))
   (not (= a (- n 1)))
   (= 1 (remainder (square a) n))))
;Miller-Rabin检查素数
(define (prime? n)
  (let((times (ceiling (/ n 2))))
    (MR-iter n times)))

(define (MR-iter n times)
  (cond ((= times 0) #t)
        ((MR-test n) (MR-iter n (- times 1)))
        (else #f)))























