#lang sicp


#|
(define (count-pairs x)
  (length (inner x '())))


(define (inner x c)
  (if(or (not (pair? x)) (memq x c))
     c
     (inner (car x)
            (inner (cdr x)
                    (cons x c)))))

|#




(define (count-pairs x)
  (define index '())
  (define (count y)
    (if(not (pair? y))
       0
       (if(memq y index)
          0
          (begin (set! index (cons y index))
                 (+ 1
                    (count (car y))
                    (count (cdr y)))))))
  (count x))

(define two (list 'a 'b))

(define three (cons two (cdr two)))

(count-pairs three)







