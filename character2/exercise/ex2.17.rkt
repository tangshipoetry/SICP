#lang racket


(define (last-pair items)
  (let ((len (length items)))
    (if(= 1 len)
       items
       (last-pair (cdr items)))))






















































