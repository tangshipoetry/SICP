#lang racket

#|(define (map p items)
  (if(null? items)
     null
     (cons (p (car items))
           (map p (cdr items)))))

(map abs (list -1 2 -5 -7 9 -63 -24))

(map (lambda(x)(* x x)) (list 1 2 3 4 5 6))|#



(define (square x)(* x x))

#|(define (square-list items)
  (if (null? items)
      null
      (cons (square (car items)) (square-list (cdr items)))))|#


(define (square-list items)
  (map square items))
































