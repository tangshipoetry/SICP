#lang racket

(define (sum-gteater x y z)
  (cond ((and (<= x y) (<= x z))(+ y z))
        ((and (<= y x) (<= y z))(+ x z))
        ((and (<= y z) (<= z x))(+ x y))))