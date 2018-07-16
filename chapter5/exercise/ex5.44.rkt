#lang racket

;网上的
(define (overwrite? operator ct-env)
  (let ((r (find-variable operator ct-env)))
    (eq? r 'not-found)))
(define (open-code? exp ct-env)
  (and (memq (car exp) '(+ - * /))
       (overwrite? (car exp) ct-env)))











































