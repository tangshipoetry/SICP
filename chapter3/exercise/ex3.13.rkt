#lang sicp

(define (last-pair x)
  (if(null? (cdr x))
     x
     (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


(define x (list 'a 'b 'c))

(define z (make-cycle x))


#|
;  ,-------------------,
;  |                   |
;  v                   |
; ( . ) -> ( . ) -> ( . )
;  |        |        |
;  v        v        v
;  'a       'b       'c
|#





















