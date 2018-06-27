#lang racket

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))


;网上答案
; The order of restrictions doesn't affect the result. but actually affect the running time. 
; for example this code is more efficient: 
(define (mutiple-dwelling) 
  (let ((baker (amb 1 2 3 4 5)) 
        (cooper (amb 1 2 3 4 5)) 
        (fletcher (amb 1 2 3 4 5)) 
        (miller (amb 1 2 3 4 5)) 
        (smith (amb 1 2 3 4 5))) 
    (require (not (= baker 5))) 
    (require (not (= cooper 1))) 
    (require (not (= fletcher 5))) 
    (require (not (= fletcher 1))) 
    (require (> miller cooper)) 
    (require (not (= (abs (- smith fletcher)) 1))) 
    (require (not (= (abs (- fletcher cooper)) 1))) 
    (require (distinct? (list baker cooper fletcher miller smith))) 
    (list (list 'baker baker) 
          (list 'cooper cooper) 
          (list 'fletcher fletcher) 
          (list 'miller miller) 
          (list 'smith smith)))) 
; Because distinct? runs in quadratic time, while other conditions can be assured in constant time. when moved it to the end of restrictions, it runs less times than before. 
; so reduce the total time. 









































