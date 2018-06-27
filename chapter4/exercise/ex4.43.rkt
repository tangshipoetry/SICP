#lang racket

#|
;网上的
本题描述的父亲名与船名的对应关系总结如下:

父亲名	船名
Mary Ann Moore A1	Lorna B1
Colonel Downing A2	Melissa B2
Mr. Hall A3	        Rosalind B3
Sir Barnacle A4	        Gabrielle B4
Dr. Parker A5	        XX B5
为了简便,后面用A1、B1 等字母来分别表示相应的父亲名与女儿名。

我这里用(女儿名 父亲名)这样的列表来表示一对父女关系,本题的已知条件是:

(B2 A4) 成立
(By Ax) 中 x!=y
(B4 Ax)、(By A5) 中,x = y
结合条件2、3,我们可以推出:

(B4 Ax) 中 x 的取值范围是1、2、3
(By A5) 中 y 的取值范围是1、2、3,由因为条件1,y 的值缩小到 1、3
本题问的是:(B1 Ax) 中 x 是多少。


|#
; 这里假设 get-index 能返回父亲或女儿的下标
; 比如 (get-index 'A1)  ==> 1
(define (get-index s)
  ...)
(define (get-father p)
  (cadr p))
(define (get-daughter p)
  (car p))

(define (pair-father)
  (let ((p1 ('B1 (amb 'A2 'A3 'A5)))
        (p2 ('B2 A4))
        (p3 ('B3 (amb 'A1 'A2 'A5)))
        (p4 ('B4 (amb 'A1 'A2 'A3)))
        (p5 ('B5 (amb 'A1 'A2 'A3))))
    (require (distinct? (get-index (get-father p1))
                        (get-index (get-father p2))
                        (get-index (get-father p3))
                        (get-index (get-father p4))
                        (get-index (get-father p5))))
    (require (distinct? (get-index (get-daughter p1))
                        (get-index (get-daughter p2))
                        (get-index (get-daughter p3))
                        (get-index (get-daughter p4))
                        (get-index (get-daughter p5))))
    (get-index (get-father p1))))
;如果没有告诉我们 Mary Ann 姓 Moore，那就是说 Lorna 在最上面表格中的位置可能是B1也可能是B5，为B1时解决的个数上面可以知道，后面再算一次为B5时的解法，最后把两种情况的解法相加即可。




























