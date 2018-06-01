#lang racket

;来自网上

因为 ((s (lambda () (* x x)))) 和 ((s (lambda () (set! x (+ x 1))))) 都是串行化操作,因此可以将它们看作是一个单独的执行单位 sa 和 sb ,并将题目给出的表达式转换成以下表示:

(parallel-execute (lambda () (set! x sa))
                  sb)




sb –> (set! x sa)
(set! x ?) –> sb –> (set! x sa)
(set! x sa) –> sb

(set! x (+ 10 1)) => x = 11 => (set! x (* 11 11)) => x = 121
(set! x ?) => (set! x (+ 10 1)) => x = 11 => (set! x (* 11 11)) => x = 121
(set! x (* 10 10)) => x = 100 => (set! x (+ 100 1)) => x = 101




























