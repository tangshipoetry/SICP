#lang racket

(define a (list 1 2 3))
(define b (list 4 5 6))
(define c (list 7 8 9))
(define d (list 10 11 12))
(define e (list a b))
(define f (list c d))
(define tree (list e f))

#|
他人分析摘抄
和换零钱问题的思路是一样的,对于一个集合的所有子集的集合,可以分为两部分,含有第一个元素和不含第一个元素的集合。而且含第一个元素的所有子集除去第一个元素,恰好正是所有不含第一个元素的子集。
也可以换个思路，对于集合A，设它可以表示为 (a1)∪(a2,...,an) ，而 (a2,...,an) 的所有子集的集合是 B=(B1,...Bm),那么可以证明A的所有子集的集合 C=B∪((A1)∪B1,(A1)∪B2,...,(A1)∪Bm);
证明：设 X 是 A 的一个子集，那么如果 a1∈X，那么 X∈((A1)∪B1,(A1)∪B2,...,(A1)∪Bm)，否则X∈B，所以
    X∈C
|#

(define (subsets s)
  (if(null? s)
     (list null)
     (let ([rest (subsets (cdr s))])
       (append rest (map (lambda(x)
                           (cons (car s)
                                   x))
                         rest)))))


(subsets a)







































