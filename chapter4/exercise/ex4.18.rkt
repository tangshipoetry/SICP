#lang racket

(lambda <vars>
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (let ((a <e1>)
          (b <e2>))
      (set! u a)
      (set! v b))
    <e3>))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)




;我们可以将solve代入本题给出的形式中
(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))

;再把内部的let展开
(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    ((lambda (a b)
       (set! y a)
       (set dy b))
     (integral (delay dy) y0 dt)
     (stream-map f y))
    y))

;在求值(stream-map f y)时,需要y的值,而这时y还是'*unassigned*,所以这时会出错;需要注意的是在求值(integral (delay dy) y0 dt)时并不会出错,因为其中的dy已经延时求值了。

;而正文中给出的形式在求值y时，y已经定义好了，所以是没问题的

(define (solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))























