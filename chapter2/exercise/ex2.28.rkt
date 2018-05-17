#lang racket

(define a (list 1 2 3))
(define b (list 4 5 6))
(define c (list 7 8 9))
(define d (list 10 11 12))
(define e (list a b))
(define f (list c d))
(define tree (list e f))

#||#
(define (fringe items)
  (if(null? items)
     '()
     (if(pair? items)
        (append (fringe (car items))
                (fringe (cdr items)))
        (list items))))




(fringe tree)

















