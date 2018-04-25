#lang racket

(define a (list 1 2 3 4))

(car a)

(cadr a)

(caddr a)

(cddddr a)


;获取序列中第n项元素
(define (list-ref items n)
  (if(= n 0)
     (car items)
     (list-ref (cdr items) (- n 1))))

;获取序列长度
;递归方式
(define (len-rec items)
  (if(null? items)
     0
     (+ 1 (len-rec (cdr items)))))
;迭代方式
(define (len-iter items)
  (define (iter s counter)
    (if(null? s)
       counter
       (iter (cdr s) (+ 1 counter))))
  (iter items 0))


(define (append list1 list2)
  (if(null? list1)
     list2
     (cons (car list1)
           (append (cdr list1) list2))))







































