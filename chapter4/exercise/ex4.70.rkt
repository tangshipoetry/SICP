#lang racket

;  原文给出方式
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

;  本题给出方式
(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok))


#|
;网上的
原文给出的方式中的let是为了对THE-ASSERTIONS进行一次求值,
如果改成本题给出的方式,那么THE-ASSERTIONS将永远只能取出第一个assertion来,
也就是说(cdr THE-ASSERTIONS) == THE-ASSERTIONS

|#

#|
;网上的
 Because we use (cons-stream assertion THE-ASSERTION),
so THE-ASSERTIONS will not be evaluated,
(set! THE-ASSERTION (cons-stream assertion THE-ASSERTIONS))
will make THE-ASSERTION in the stream point to itself.
so if we use THE-ASSERTIONS, it will lead to infinite loop. 
|#







































