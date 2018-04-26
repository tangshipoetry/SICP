#lang racket

(define a (list 1 2 3))
(define b (list 4 5 6))
(define c (list 7 8 9))
(define d (list 10 11 12))
(define e (list a b))
(define f (list c d))
(define tree (list e f))


#|
(define (reverse items)
  (define result '())
  (define (iter list r)
    (if(null? list)
       r
       (iter (cdr list)
             (cons (car list) r))))
  (iter items result))
|#



#|;双重递归————引用网上
(define (deep-reverse tree)
  (define (iter items result)
    (if(null? items)
       result
       (iter (cdr items)
             (cons (if(pair? (car items))
                      (deep-reverse (car items))
                      (car items))
                   result))))
  (iter tree '()))|#

#|
;利用自带;list?过程判断
(define (deep-reverse tree)
  (if(null? tree)
     '()
     (append (deep-reverse (cdr tree))
             (if(list? (car tree))
                (list (deep-reverse (car tree)))
                (list (car tree))))))
|#


#|(cons (deep-reverse (cdr tree))
             (deep-reverse (car tree)))|#


;使用reverse和map————引用网上
(define (deep-reverse items)
  (reverse (map (lambda(x)
                  (if(pair? x)
                     (deep-reverse x)
                     x))
                items)))


(deep-reverse tree)










































