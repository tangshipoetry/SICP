#lang racket


(define (element-of-set? x set)
  (cond((null? set) #f)
       (= x (car set) #t)
       (< x (car set) #f)
       (else (element-of-set? x (cdr set)))))


(define (intersection-set set1 set2)
  (if(or (null? set1) (null? set2))
     null
     (let ([x1 (car set1)][x2 (car set2)])
       (cond((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((> x1 x2) (intersection-set set1 (cdr set2)))
            ((< x1 x2) (intersection-set (cdr set1) set2))))))

(define (adjoin x set)
  (if(null? set)
     (list x)
     (let ([c (car set)][r (cdr set)])
       (cond ((= x c) set)
             ((< x c) (cons x set))
             ((> x c) (cons c (adjoin x r)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ([x1 (car set1)][x2 (car set2)])
           (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))






















































