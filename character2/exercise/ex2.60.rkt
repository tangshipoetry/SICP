#lang racket


(define (element-of-set? x set)
  (cond((null? set) #f)
       ((equal? (car set) x) #t)
       (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))



(define (intersection-set set1 set2)
  (define (iter set result)
    (cond((null? set) result)
         ((element-of-set? (car set) set2)
          (if(element-of-set? (car set) result)
             (iter (cdr set) result)
             (iter (cdr set) (cons (car set) result))))
         (else (iter (cdr set) result))))
  (if(or (null? set1) (null? set2))
     null
     (iter set1 null)))














































