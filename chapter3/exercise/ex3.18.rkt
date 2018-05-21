#lang sicp

#|
(define (loop? lst)
  (let ((identity (cons '() '())))
    (define (iter remain-list)
      (cond ((null? remain-list)
             #f)
            ((eq? identity (car remain-list))
             #t)
            (else
             (set-car! remain-list identity)
             (iter (cdr remain-list)))))
    (iter lst)))
|#



(define (last-pair x)
  (if(null? (cdr x))
     x
     (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define x (list 'a 'b 'c))

(define z (make-cycle x))



(define (contain-loop? x)
  (define index '())
  (define(loop? lst)
    (cond ((null? lst) #f)
          ((not (pair? lst)) #f)
          ((memq lst index) #t)
          (else
           (set! index (cons lst index))
           (or (loop? (car lst))
               (loop? (cdr lst))))))
  (loop? x))























