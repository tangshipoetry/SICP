#lang racket

(define (gcd a b)
  (if(= b 0)
     a
     (gcd b
          (remainder a
                     b))))

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

;自己写的
(define (make-rat x y)
  (let ((c (abs (gcd x y)))(abs-numer (abs x))(abs-denom (abs y)))
    (if(< 0 (* x y))
       (cons (/ (- abs-numer) c) (/ abs-denom c))
       (cons (/ abs-numer c) (/ abs-denom c)))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))


#|
别人的答案
(define (make-rat n d)
    (if (< d 0)
        (cons (- n) (- d))
        (cons n d)))|#













