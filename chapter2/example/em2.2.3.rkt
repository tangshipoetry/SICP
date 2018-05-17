#lang racket

(define (odd? x)
  (= 1 (remainder x 2)))
(define (even? x)
  (= 0 (remainder x 2)))

(define a (list 1 2 3))
(define b (list 4 5 6))
(define c (list 7 8 9))
(define d (list 10 11 12))
(define e (list a b))
(define f (list c d))
(define tree (list e f))

;斐波那契
;steps(time)指数增长,space线性增长
#|(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))|#

;time space liner increase
(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b counter)
  (if(= counter 0)
     b
     (fib-iter (+ a b) a (- counter 1))))

;过滤器
(define (filter p sequence)
  (if(null? sequence)
     '()
     (if(p (car sequence))
        (cons (car sequence)
              (filter p (cdr sequence)))
        (filter p (cdr sequence)))))

;操作累计
(define (accumulate op initial sequence)
  (if(null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

(define (square x)
  (* x x ))

;interval enumerate
(define (enumerate-interval lower high)
  (if(> lower high)
     null
     (cons lower
           (enumerate-interval (+ 1 lower)
                               high))))

;enumarate tree's leaves
#|
(define (enumearte-tree tree)
  (if(null? tree)
     null
     (if(pair? (car tree))
        (append (enumearte-tree (car tree))
                (enumearte-tree (cdr tree)))
        (cons (car tree)
              (enumearte-tree (cdr tree))))))
|#
(define (enumearte-tree tree)
  (if(null? tree)
     null
     (if(pair? tree)
        (append (enumearte-tree (car tree))
                (enumearte-tree (cdr tree)))
        (list tree))))



(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumearte-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              null
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))































