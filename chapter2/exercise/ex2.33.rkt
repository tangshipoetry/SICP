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




(define (map p sequence)
  (accumulate (lambda(x y)
                (cons (p x)
                      y))
              null
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda(x y)
                (if(null? x)
                   0
                   (+ 1 y)))
              0
              sequence))

























