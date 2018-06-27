#lang racket

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))


(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-end-before n)
  (amb n (an-integer-end-before (- n 1))))


;自己写的
(define (an-integer-between low high)
  (let(x (an-integer-starting-from low))
    (an-integer-end-before high)))





;网上的
(define (an-interger-between low high)  
  (require (<= low high))  
  (amb low (an-interger-between (+ low 1) high))) 





(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j))
                    (* k k)))
        (list i j k)))))



































