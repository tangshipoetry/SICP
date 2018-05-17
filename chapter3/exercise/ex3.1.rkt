#lang racket

(define (make-accumulator num)
  (lambda(x)
    (begin
      (set! num (+ num x))
      num)))


(define a (make-accumulator 10))

(define b (make-accumulator 20))


































