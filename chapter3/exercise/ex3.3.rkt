#lang racket

#|
(define (make-account balance)
  (define (withdraw amount)
    (if(>= balance amount)
       (begin
         (set! balance (- balance amount))
         balance)
       "balance is not enough"))
  (define (deposit amount)
    (begin
      (set! balance (+ balance amount))
      balance))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "unknow request--make-account" m))))
  dispatch)
|#


(define (make-account balance password)
  (define (withdraw amount p)
    (if(eq? p password)
       (if(>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "balance is not enough")
       "Incorrect password"))
  (define (deposit amount p)
    (if(eq? p password)
       (begin
         (set! balance (+ balance amount))
         balance)
       "Incorrect password"))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "unknow request--make-account" m))))
  dispatch)







































