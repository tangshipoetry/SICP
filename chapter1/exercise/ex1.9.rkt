#lang racket
(define (inc c)
  (+ c 1))
(define (dec c)
  (- c 1))
#|(define (add a b)
  (if (= a 0)
      b
      (inc (add (dec a) b))))|#

(define (add a b)
  (if(= a 0)
     b
     (add (dec a) (inc b))))













