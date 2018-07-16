#lang racket

;网上的
(define (find-variable variable lst) 
  (define (search-variable v l n) 
    (cond ((null? l) false) 
          ((eq? v (car l)) n) 
          (else (search-variable v (cdr l) (+ n 1))))) 
  (define (search-frame frames f) 
    (if (null? frames) 
        'not-found 
        (let ((o (search-variable variable (car frames) 0))) 
          (if o 
              (cons f o) 
              (search-frame (cdr frames) (+ f 1)))))) 
  (search-frame lst 0)) 

































