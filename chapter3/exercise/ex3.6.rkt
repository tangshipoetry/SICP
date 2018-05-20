#lang racket

(define (rand order)
  (cond((eq? order 'generate)(random))
       ((eq? order 'reset)
        (lambda(seed)(random-seed seed)))
       (else (error "Unknow-order---rand" order))))



(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(display "----------------------------------------------------------------")
((rand 'reset) 7)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(display "----------------------------------------------------------------")
((rand 'reset) 7)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)
(rand 'generate)

















