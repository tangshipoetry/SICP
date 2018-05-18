#lang racket

(define (make-account password balance)
  (define (call-the-cop) "call-the-cop")
  (define counter 0)
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
       (lambda(amount)
         (set! counter (+ 1 counter))
         (if(>= counter 7)
            (call-the-cop)
            "incorrect password"))))
  dispatch)



(define acc (make-account 'tangshi 100))

((acc 'tangshi 'withdraw) 20)

((acc 'tangshi 'deposit) 10)


((acc 'tangsh 'withdraw) 20)
((acc 'tangsh 'withdraw) 20)
((acc 'tangsh 'withdraw) 20)
((acc 'tangsh 'withdraw) 20)
((acc 'tangsh 'withdraw) 20)
((acc 'tangsh 'withdraw) 20)
((acc 'tangsh 'withdraw) 20)




































