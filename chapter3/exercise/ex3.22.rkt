#lang sicp


(define (make-queue)
  (let([front-ptr '()]
       [rear-ptr '()])
    
    (define (empty-queue?) (null? front-ptr))
    
    (define (insert-queue! item)
      (let([new-pair (list item)])
        (cond((empty-queue?)
              (set! front-ptr new-pair)
              (set! rear-ptr new-pair)
              front-ptr)
             (else
              (set-cdr! rear-ptr new-pair)
              (set! rear-ptr new-pair)
              front-ptr))))

    (define (delete-queue!)
      (cond((empty-queue?)
            (error "FRONT called with an empty queue"))
           (else
            (set! front-ptr (cdr front-ptr))
            front-ptr)))
    
    (define (dispatch m)
      (cond((eq? m 'front-ptr) front-ptr)
           ((eq? m 'rear-ptr) rear-ptr)
           ((eq? m 'insert-queue!) insert-queue!)
           ((eq? m 'delete-queue!) (delete-queue!))
           ((eq? m 'empty-queue?) empty-queue?)
           (else
            (error "Unknow operation -- DISPATCH" m))))

    dispatch))

#|

|#



(define q (make-queue))
((q 'insert-queue!) 'a)
((q 'insert-queue!) 'b)
((q 'insert-queue!) 'c)
(q 'delete-queue!)
(q 'delete-queue!)
(q 'delete-queue!)















