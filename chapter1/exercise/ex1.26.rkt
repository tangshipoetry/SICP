#lang racket






;不用square，展开后相当于正则，原本已经计算出来的值又计算一次，这样每次遇到exp为偶数时，计算量增加一倍
(remainder (* (expmod base (/ exp 2) m)
              (expmod base (/ exp 2) m))
           m)














