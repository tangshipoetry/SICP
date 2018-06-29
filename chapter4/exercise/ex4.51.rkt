#lang racket


(define count 0)

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

(a b 2)

try-again

(a c 3)
如果这里使用set!,那么会输出

(a b 1)

try-again

(a c 1)
因为这里每次求值失败时，都会将 count 取消之前对其的赋值。















































