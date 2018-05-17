#lang racket


(define (square x)
  (* x x))

(define (smallest-divison n)
  (find-divisor n 2))

(define (find-divisor n x)
  (cond((> (square x) n) n)
       ((divisor? n x) x)
       (else (find-divisor n (+ x 1)))))
(define (divisor? n x)
  (= 0 (remainder n x)))

(define (prime? n)
  (= n (smallest-divison n)))


(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if(prime? n)
     (report-prime (- (current-milliseconds) start-time))
     (display "")))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (even? x)
  (= 0 (remainder x 2)))





;获取下一个奇数
(define (next-odd x)
  (if(= 0 (remainder x 2))
     (+ 1 x)
     (+ 2 x)))


(define (search-for-primes n counter)
  (cond ((= counter 0) (display "finished"))
        ((prime? n)
         (newline)
         (timed-prime-test n)
         (search-for-primes (next-odd n) (- counter 1)))
        (else (search-for-primes (next-odd n) counter))))































