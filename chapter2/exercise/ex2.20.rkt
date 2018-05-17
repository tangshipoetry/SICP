#lang racket

(define (even? x)
  (= 0 (remainder x 2)))
(define (odd? x)
  (= 1 (remainder x 2)))

(define (same-parity first . items)
  (if(null? items)
     '()
     (if(even? first)
        (cons first (filter-even items))
        (cons first (filter-odd items)))))



;提取表中所有偶数
(define (filter-even items)
  (if(null? items)
     items
     (let ((item (car items)))
       (if(even? item)
          (cons item (filter-even (cdr items)))
          (filter-even (cdr items))))))

;提取表中所有奇数
(define (filter-odd items)
  (if(null? items)
     items
     (let ((item (car items)))
       (if(odd? item)
          (cons item (filter-odd (cdr items)))
          (filter-odd (cdr items))))))













































