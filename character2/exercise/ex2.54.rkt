#lang racket


(define (equal? a b)
  (cond((and (pair? a)(pair? b))
        (and (eq? (car a) (car b))
             (equal? (cdr a) (cdr b))))
       ((and (not (pair? a))
             (not (pair? b)))
        (eq? a b))
       (else #f)))









































