#lang racket

(define (square x)(* x x))

#|
(define (square-tree tree)
  (map (lambda(x)
         (if(null? x)
            '()
            (if(pair? x)
               (square-tree x)
               (square x))))
       tree))

|#


(define (square-tree tree)
  (tree-map square tree))

(define (tree-map f tree)
  (map (lambda(x)
         (if(null? x)
            '()
            (if(pair? x)
               (tree-map f x)
               (f x))))
       tree))





(define a (list 1 2 3))
(define b (list 4 5 6))
(define c (list 7 8 9))
(define d (list 10 11 12))
(define e (list a b))
(define f (list c d))
(define tree (list e f))


(square-tree tree)



































