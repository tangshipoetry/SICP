#lang racket

;原版
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))



;题目
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave
       (stream-car stream)
       (flatten-stream (stream-cdr stream)))))

;网上的
;如果interleave-delayed的第二个参数没用delay,很显然是不能处理无穷流的,和习题4.71差不多。


































