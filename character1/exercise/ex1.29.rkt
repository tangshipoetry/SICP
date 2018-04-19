#lang racket

;simpson-method

(define (even? x)
  (= 0 (remainder x 2)))


(define (inc a)
  (+ 1 a))

(define (sum term a next b)
  (if(> a b)
     0
     (+ (term a)
        (sum term (next a) next b))))



(define (simpson-sum f a b n)
  (define h (/ (- b a) n))
  (define (spsc m)
  (cond ((or (= 0 m) (= n m)) 1)
        ((even? m) 2)
        (else 4)))
  (define (factor x)(f (+ a (* x h))))
  (define (simpson-term x)
    (* (spsc x) (factor x)))
  (if(not (even? n))
     (error "n can't be odd")
     (* (/ h 3.0) (sum simpson-term 0 inc n))))




(define (cube x)(* x x x))























