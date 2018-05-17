#lang racket

#|
(define (withdraw amount)
  (let([balance 100])
    (if(>= balance amount)
       (begin
         (set! balance (- balance amount))
         balance)
       "balance is not enough")))
|#

#|
(define (new-withdraw balance)
  (lambda(amount)
    (if(>= balance amount)
       (begin
         (set! balance (- balance amount))
         balance)
       "balance is not enough")))
|#

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





















