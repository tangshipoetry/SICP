#lang racket

#|
;'((((() . 1) . 4) . 9) . 16)|#

;修改后
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                    (list (square (car things)))))))
  (iter items null))


(define (square x)(* x x))


#|(define (square-list items)
  (define (iter things answer)
    (if(null? things)
       answer
       (iter (cdr things)
             (cons (square (car things))
                   answer))))
  (iter items null))|#









(define a (list 1 2 3 4))

































