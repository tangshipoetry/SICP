#lang racket


;网上的

利用本小节提供的惰性求值的功能,产生的流的car与cdr这两部分都是延时的值,这是与第3章中给出的流的本质不同。

solve 就利用了这一点，

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y)))
在计算y时需要dy的值，而dy的值这时还没有计算出来，而采用本小节给出的求值器，就可以延时求dy的值了。







































