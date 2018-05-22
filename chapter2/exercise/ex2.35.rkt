#lang racket

;过滤器
(define (filter p sequence)
  (if(null? sequence)
     '()
     (if(p (car sequence))
        (cons (car sequence)
              (filter p (cdr sequence)))
        (filter p (cdr sequence)))))

;interval enumerate
(define (enumerate-interval lower high)
  (if(> lower high)
     null
     (cons lower
           (enumerate-interval (+ 1 lower)
                               high))))

;操作累计
(define (accumulate op initial sequence)
  (if(null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))


(define (enumerate-tree tree)
  (if(null? tree)
     null
     (if(pair? tree)
        (append (enumerate-tree (car tree))
                (enumerate-tree (cdr tree)))
        (list tree))))



(define a (list 1 2 3))
(define b (list 4 5 6))
(define c (list 7 8 9))
(define d (list 10 11 12))
(define e (list a b))
(define f (list c d))
(define tree (list e f))



;摘自网上
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda(x)
                     (if(null? x)
                        0
                        (if(pair? x)
                           (count-leaves x)
                           1))) t)))

(count-leaves tree)



























