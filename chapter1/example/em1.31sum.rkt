#lang racket

(define (sum term a next b)
  (if(> a b)
     0
     (+ (term a)
        (sum term (next a) next b))))

(define (inc a)
  (+ 1 a))

(define (cube x)
  (* x x x))

(define (integer x) x)

(define (sum-integer a b)
  (sum integer a inc b))

(define (sum-cube a b)
  (sum cube a inc b))


(define (pi-sum a b)
  (define (pi-next a)(+ a 4))
  (define (pi-term a)
    (/ 1.0
       (* a (+ a 2))))
  (sum pi-term a pi-next b))

(define (pi m)
  (* 8 (pi-sum 1 m)))

(define (integral f a b dx)
  (define (add-dx a)
    (+ a dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
















