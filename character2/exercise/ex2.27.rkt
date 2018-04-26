#lang racket

(define (reverse items)
  (define result '())
  (define (iter list r)
    (if(null? list)
       r
       (iter (cdr list)
             (cons (car list) r))))
  (iter items result))









(define a (list 1 2 3))
(define b (list 4 5 6))
(define c (list a b))










































