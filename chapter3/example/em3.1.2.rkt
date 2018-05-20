#lang sicp

(define (rand)
  (random 4294967087))

(define (mento-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (if(= 0 trials-remaining)
       (/ trials-passed trials)
       (if(experiment)
          (iter (- trials-remaining 1) (+ trials-passed 1))
          (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (cesaro-test)
  (= 1 (gcd (rand) (rand))))

(define (estimate-pi trails)
  (sqrt (/ 6 (mento-carlo trails cesaro-test))))























