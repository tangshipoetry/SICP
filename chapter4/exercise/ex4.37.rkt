#lang racket

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))


;网上答案

#|

相比 4.35 中的方法,本题中的方法少了一层不确定求值。

(require (>= hsq ksq)) 限制了 i,j的范围比 4.35 的要小

|#


























