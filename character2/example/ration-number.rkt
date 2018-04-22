#lang racket

(define (gcd a b)
  (if(= b 0)
     a
     (gcd b
          (remainder a
                     b))))

(define (make-rat x y)
  (let ((c (gcd x y)))
    (cons (/ x c) (/ y c))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))


(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))


(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat x y)
   (= (* (numer x) (denom y))
      (* (denom x) (numer y))))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))







