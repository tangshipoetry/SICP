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


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond((= times 0) #t)
       ((fermat-test n)(fast-prime? n (- times 1)))
       (else #f)))

(define (prime? n)
  (fast-prime? n 10))

(define (next-test x)
  (if(= x 2)
     3
     (+ 2 x)))



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


(define (search-for-primes n counter)
  (cond ((= counter 0)(newline)(display "finished"))
        ((prime? n)
         (newline)
         (timed-prime-test n)
         (search-for-primes (next-test n) (- counter 1)))
        (else (search-for-primes (next-test n) counter))))






























