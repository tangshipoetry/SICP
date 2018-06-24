#lang racket


;网上答案
如果有的话,那么下面的程序是能够运行的

(define (run-forever)
  (run-forever))

(define (try p)
  (if (halts? p p)
    (run-forever)
    'halted'))

(try try)    
这个是著名的“停机问题”，上面的try是自相矛盾的，因为：

假设try能够中止的，那么(halts? p p)为true，那么会执行(run-forever)，这时陷入死循环，与一开始的假设矛盾
假设try能不够中止的，那么(halts? p p)为false，那么会执行'halted，这时try运行结束，与一开始的假设矛盾






























s