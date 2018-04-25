#lang racket


(define (last-pair items)
  (let ((len (length items)))
    (if(= 1 len)
       items
       (last-pair (cdr items)))))


(define (reverse items)
  (define result '())
  (define (iter list r)
    (if(null? list)
       r
       (iter (cdr list)
             (cons (car list) r))))
  (iter items result))



(define a (list 1 2 3 4 5))





























