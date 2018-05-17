#lang racket

;操作累计
(define (fold-right op initial sequence)
  (if(null? sequence)
     initial
     (op (car sequence)
         (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if(null? rest)
       result
       (iter (op result (car rest))
             (cdr rest))))
  (iter initial sequence))

#|
(define (reverse sequence)
  (fold-right (lambda(x y)
                (append y
                        (list x)))
              null
              sequence))
|#


(define (reverse sequence)
  (fold-left (lambda(x y)
               (cons y x))
             null
             sequence))




(define a (list 1 2 3))
(define b (list 4 5 6))
(define c (list 7 8 9))
(define d (list 10 11 12))
(define e (list a b))
(define f (list c d))
(define tree (list e f))

(reverse a)











































