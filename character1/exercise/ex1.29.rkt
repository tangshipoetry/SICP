#lang racket

;simpson-method

(define (even? x)
  (= 0 (remainder x 2)))

(define (inc a)
  (+ 1 a))

(define (spsc m)
  (cond ((= 1 m) 1)
        ((even? m) 4)
        (else 2)))

(define (sum term a next b)
  (if(> a b)
     0
     (+ (term a)
        (sum term (next a) next b))))

(define (simpson-term counter f a h)
  (* (spsc counter)
     (f (+ a (* counter h)))))
































