#lang sicp

#|
;自己写的

(define (cons-item prv-item-ptr item)
  (cons item prv-item-ptr))
(define (get-prv-ptr item)
  (cdr item))
(define (get-item item)
  (car item))
(define (set-prv! item prv)
  (set-cdr! item prv))

(define (make-deque)
  (cons '() '()))

(define (empty-deque? deque)
  (null? (front-deque-ptr deque)))

(define (front-deque-ptr deque)
  (car deque))
(define (rear-deque-ptr deque)
  (cdr deque))
(define (front-deque deque)
  (if(empty-deque? deque)
     (error "front-deque called with an empty deque" deque)
     (car (front-deque-ptr deque))))

(define (set-front-ptr! deque item)
  (set-car! deque item))
(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (front-insert-deque! deque item)
  (define new-pair (cons-item '() item))
  (cond((empty-deque? deque)
        (set-front-ptr! deque (list new-pair))
        (set-rear-ptr! deque (list new-pair))
        deque)
       (else
        (set-prv! (front-deque deque) new-pair)
        (set-front-ptr! deque (cons new-pair (front-deque-ptr deque)))
        deque)))

(define (rear-insert-deque! deque item)
  (define new-pair (cons-item '() item))
  (cond((empty-deque? deque)
        (set-front-ptr! deque (list new-pair))
        (set-rear-ptr! deque (list new-pair))
        deque)
       (else
        (let([rear (list new-pair)])
          (set-cdr! (rear-deque-ptr deque) rear)
          (set-prv! new-pair (rear-deque-ptr deque))
          (set-rear-ptr! deque rear)
          deque))))

(define (front-delete-deque! deque)
  (cond((empty-deque? deque)
        (error "front-delete-deque! called with an empty deque" deque))
       (else
        (let ([new-first (car (cdr (front-deque-ptr deque)))])
          (set-prv! new-first '())
          (set-front-ptr! deque new-first)
          deque))))

(define (rear-deletedeque! deque)
  (cond((empty-deque? deque)
        (error "front-delete-deque! called with an empty deque" deque))
       (else
        (set-rear-ptr! deque (get-prv-ptr (car (rear-deque-ptr deque))))
        (set-cdr! (rear-deque-ptr deque) '())
        deque)))

(define (dqprint deque)
  (if(or (empty-deque? deque)
         (null? deque))
     '()
     (begin (get-item (car deque))
            (dqprint (cdr deque)))))



(define dq (make-deque))
;(dqprint dq)
(front-insert-deque! dq 'a)
;(dqprint dq)
(rear-insert-deque! dq 'b)
;(dqprint dq)
(front-insert-deque! dq 'c)
;(dqprint dq)
(front-delete-deque! dq)
;(dqprint dq)
(rear-deletedeque! dq)
|#



;网上的
;;; We gonna make a two way structure like this diagram 
 ;;;                 +-------------------+ 
 ;;; +---------------|------+            | 
 ;;; +->[[a|/] | -]--+->[[b|'] | -]->[[c|'] |/] 
 ;;; 
 ;;; Which will display this in the repl of guile scheme 
 ;;; ((a) (b . #-2#) (c . #-2#)) 
  
(define (make-deque) 
  (let ((front-ptr '()) 
        (rear-ptr '())) 
    (define (dispatch m) 
      (cond ((equal? m 'empty-deque?) 
             (null? front-ptr)) 
            ((equal? m 'rear-ptr) 
             rear-ptr) 
            ((equal? m 'front-ptr) 
             front-ptr) 
            ((equal? m 'set-front-ptr!) 
             (lambda (item) (set! front-ptr item))) 
            ((equal? m 'set-rear-ptr!) 
             (lambda (item) (set! rear-ptr item))))) 
    dispatch)) 
  
(define (empty-deque? deque) 
  (deque 'empty-deque?)) 
(define (front-ptr deque) 
  (deque 'front-ptr)) 
(define (rear-ptr deque) 
  (deque 'rear-ptr)) 
(define (front-deque deque) 
  (if (empty-deque? deque) 
      (error "FRONT called with an empty deque" deque) 
      (caar (front-ptr deque)))) 
(define (rear-deque deque) 
  (if (empty-deque? deque) 
      (error "REAR called with an empty deque" deque) 
      (caar (rear-ptr deque)))) 
(define (set-front-ptr! deque item) 
  ((deque 'set-front-ptr!) item)) 
(define (set-rear-ptr! deque item) 
  ((deque 'set-rear-ptr!) item)) 
  
(define (front-insert-deque! deque item) 
  (let ((newlist (cons (cons item '()) '()))) 
    (if (empty-deque? deque) 
        (begin 
          (set-front-ptr! deque newlist) 
          (set-rear-ptr! deque newlist) 
          deque) 
        (begin 
          (set-cdr! (car (front-ptr deque)) newlist) 
          (set-cdr! newlist (front-ptr deque)) 
          (set-front-ptr! deque newlist) 
          deque)))) 
  
(define (rear-insert-deque! deque item) 
  (let ((newlist (cons (cons item '()) '()))) 
    (if (empty-deque? deque) 
        (begin 
          (set-front-ptr! deque newlist) 
          (set-rear-ptr! deque newlist) 
          deque) 
        (begin 
          (set-cdr! (car newlist) (rear-ptr deque)) 
          (set-cdr! (rear-ptr deque) newlist) 
          (set-rear-ptr! deque newlist) 
          deque)))) 
  
(define (front-delete-deque! deque) 
  (cond ((empty-deque? deque) 
         (error "DELETE! called with an empty deque" deque)) 
        ((null? (cdar (rear-ptr deque))) 
         (set-front-ptr! deque '())) 
        (else 
         (set-front-ptr! deque (cdr (front-ptr deque))) 
         (set-cdr! (car (front-ptr deque)) '()) 
         deque))) 
  
(define (rear-delete-deque! deque) 
  (cond ((empty-deque? deque) 
         (error "DELETE! called with an empty deque" deque)) 
        ((null? (cdar (rear-ptr deque))) 
         (set-front-ptr! deque '())) 
        (else 
         (set-rear-ptr! deque (cdar (rear-ptr deque))) 
         (set-cdr! (rear-ptr deque) '())))) 
  
(define (print-deque deque) 
  (display (map car 
                (front-ptr deque))) (newline)) 


