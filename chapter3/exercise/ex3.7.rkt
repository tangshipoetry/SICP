#lang racket

;ex3.3
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


(define peter-acc (make-account 'open-sesame 100))



(define (make-joint acc password new-password)
  (lambda(p m)
    (if(eq? p 'rosebud)
       (peter-acc 'open-sesame m)
       (lambda(amount) "incorrect password"))))


(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))



((paul-acc 'rosebud 'withdraw) 20)
((paul-acc 'roebud 'withdraw) 20)


 












