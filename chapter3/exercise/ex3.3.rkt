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


#|
(define (make-account password balance)
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
  (define (dispatch p m)
    (cond ((eq? m 'withdraw) (withdraw ))
          ((eq? m 'deposit) deposit)
          (else (error "unknow request--make-account" m))))
  dispatch)
|#



(define (make-account password balance)
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
  (define (dispatch p m)
    (if(eq? p password)
       (cond ((eq? m 'withdraw) withdraw)
             ((eq? m 'deposit) deposit)
             (else (error "unknow request--make-account" m)))
       (lambda(amount) "incorrect password")))
  dispatch)

(define acc (make-account 'tangshi 100))

((acc 'tangshi 'withdraw) 20)

((acc 'tangshi 'deposit) 10)



























