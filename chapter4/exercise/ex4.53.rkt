#lang racket

(let ((pairs '()))
  (if-fail
   (let ((p (prime-sum-pair '(1 3 5 8)
                            '(20 35 110))))
     (permanent-set! pairs (cons p pairs))
     (amb))
   pairs))




;网上的答案

;;; Starting a new problem 
;;; Amb-Eval value:
((8 35) (3 110) (3 20))

;;; Amb-Eval input:
try-again

;;; There are no more values of
(let ((pairs '())) (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110)))) (permanent-set! pairs (cons p pairs)) (amb)) pairs))




























