#lang racket

(define (make-monitored f)
  (define counter 0)
  (lambda(m)
    (cond((eq? m 'how-many-calls) counter)
         ((eq? m 'reset-count) (set! counter 0))
         (else
          (set! counter (+ 1 counter))
          (f m)))))





(define 1+ (lambda(x)(+ 1 x)))



















































