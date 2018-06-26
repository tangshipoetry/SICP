#lang racket

(define (unless condition usual-value exceptional-value)
  (if condition
      exceptional-value
      usual-value))

((unless? exp)(eval (unless->if exp) env))

(define (unless? expr) (tagged-list? expr 'unless))

(define (unless-predicate exp) (cadr exp))

(define (unless-consequnce expr)
  (if (not (null? (cdddr expr)))
      (cadddr expr)
      'false))

(define (unless-alternative exp)(caddr exp))

(define (unless->if expr)
  (make-if (unless-predicate expr)
           (unless-consequence expr)
           (unless-alternative expr)))





#|
Ben 的观点是将unless实现为一种特殊形式,这么做在应用序语言中可行是因为特殊形式的求值规则与复合过程求职规则不一样,可以由解释器实现者自己定义。
Alyssa 则认为，如果把unless实现为特殊形式，那么unless就不能够与其他高阶过程结合在一起工作了。这是显而易见的，因为高阶过程可以传入或传出过程，而不是特殊形式。
|#

;如果unless可以用作一个过程而不是特殊形式,在高阶过程中就可以使用。
(define (foo f pred v1 v2)
  (f pred v1 v2))
(foo unless (> 3 2) 3 2)




















