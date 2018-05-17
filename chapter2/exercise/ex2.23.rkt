#lang racket
#|(define (map p items)
  (if(null? items)
     null
     (cons (p (car items))
           (map p \(cdr items)))))|#


(define (for-each p items)
  (if(null? items)
     (void)
     (begin (p (car items))
      (for-each p (cdr items)))))

(define a (list 1 2 3 4))


(define (print x)
  (newline)
  (display x))





































