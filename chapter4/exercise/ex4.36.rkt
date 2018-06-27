#lang racket

(define (require p) (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (my-pythagorean-triple)
  (let((k (an-integer-starting-from 1)))
    (let((i (an-integer-starting-from 1)))
      (let((j (an-integer-starting-from i)))
        (require (< k (+ i j))
        (= (+ (* i i) (* j j))
                    (* k k)))))))



;网上的
#|
假设用an-integer-starting-from替换an-integer-between的话,由于an-integer-starting-from能够产生出一个无穷序列,在产生第一个合适的毕达哥拉斯三元组后,k的值一直增加(这里假设采用书中介绍的深度优先的搜索算法),而这时永远不能满足毕达哥拉斯的条件,造成假死现象。

根据上面的分析，要想生成所有的毕达哥拉斯三元组，必须保障i、j、k三个变量中至少两个有上界，这样就不至于出现死循环了。

我们知道，k肯定是i、j、k这三者中最大的，如果k的值能够确定出来，那么i、j的上界也就自然确定了。有了这个思路我们就可以写出下面的一种解决方案：
|#

(define (all-pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
    (let ((j (an-integer-between 1 k)))
      (let ((i (an-integer-between 1 j)))
        (require (= (+ (* i i) (* j j))
                    (* k k)))
        (list i j k)))))





































