#lang racket


(define (last-pair items)
  (let ((len (length items)))
    (if(= 1 len)
       items
       (last-pair (cdr items)))))

;自己写的
#|(define (reverse items)
  (define result '())
  (define (iter list r)
    (if(null? list)
       r
       (iter (cdr list)
             (cons (car list) r))))
  (iter items result))|#



;网上查的、
(define (reverse items)
  (if(null? items)
     items
     (append (reverse (cdr items))
             (list (car items)))))



(define a (list 1 2 3 4 5))





























