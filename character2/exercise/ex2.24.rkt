#lang racket

;计算树结构中叶子个数 
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


(define a (list 1 (list 2 (list 3 4))))

(length a)

(count-leaves a)




 (1 (2 (3 4)))       ((2 (3 4)))
[*]---------------> [*]-------'()
 |                   |
 |                   |
 v                   v (2 (3 4))         ((3 4))
 1                  [*]---------------> [*]-------'()
                     |                   |
                     |                   |
                     v                   v (3 4)             (4)
                     2                  [*]---------------> [*]---------------> '()
                                         |                   |
                                         |                   |
                                         v                   v
                                         3                   4

































