#lang racket

(define (cube x)(* x x x))
(define (square x)(* x x))
(define (inc x)(+ 1 x))

(define (compose f g)
  (lambda(x)
    (f (g x))))

(define (f x)
  ((compose square inc) x))



























