#lang racket

(define (make-from-real-img x y)
  (define (dispatch op)
    (cons ((eq? 'real-part) x)
          ((eq? 'imag-part) x)
          ((eq? 'magnitude) (sqrt (+ (square x)
                                     (square y))))
          ((eq? 'abgle) (atan y x))))
  dispatch)

(define (make-from-mag-ang m a)
  (define (dispatch op)
    (cons ((eq? 'real-part) (cos m a))
          ((eq? 'imag-part) (sin m a))
          ((eq? 'magnitude) m)
          ((eq? 'abgle) a)))
  dispatch)





























