#lang racket


(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))



(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))




;网上参考
 In applicative-order Scheme, when call (factorial 5), the call will not end. because,
when call unless, even if (= n 1) is true, (factorial (- n 1)) will be called. so n will be 5, 4, 3, 2, 1, 0, -1 .... .
In normal-order Scheme, this will work, Because normal-order Scheme uses lazy evaluation, when (= n 1) is true,
(factorial n) will not be called. 


在应用序的语言中,在应用 unless 过程时,会依次计算出其所有参数后,在进入函数体,而上面的 factorial 由于参数中引用了自身,所以会导致死循环

但在正则序的语言中，参数是到最后真正用到时才求值，就能够避免这个问题。




























