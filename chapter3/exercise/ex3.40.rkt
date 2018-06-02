#lang racket

;来自网上

以上解释符号不太清晰

P:(set! x (* x x))``应理解为两次读取``x``与一次写入``x,我们分别记作r11,r12,w1 Q:(set! x (* x x x))``应理解为三次读取``x``与一次写入``x,我们分别记作r21,r22,r23,w2

在保持r11，r12，w1和r21，r22，r23，w2各自内部顺序不变，可以交错排序，因此有 7!/(3!4!) = 35 种不同排法

但考虑到正真影响最后结果的却是 （1）w1，w2 的顺序 （2）w1，r21，r22，r23 的顺序 （3）w2，r11，r12 的顺序

考虑（1）将影响最后的写入操作、（2）将影响Q的读取操作进而影响w2、（3）将影响P的读取操作进而影响w1

分两类

类一：最后完成写操作的是w1，因此它始终在r21，r22，r23之后，所以没有（2）的影响，也就不会影响w2的结果，w2 == 1,000，这时只考虑（3） （i） w2-r11-r12：r11和r12都读取 x=1,000，故 w1 = 1,000,000 （ii） r11-w2-r12：r11读取 x=10，r12读取，x=1,000，故 w1 = 10,000 （iii） r11-r12-w2：r11和r12都读取 x=10，故 w1 = 100

类二：最后完成写操作的是w2，因此它始终在r11，r12之后，所以没有（3）的影响，也就不会影响w1的结果，w1 == 100，这时只考虑（2） （i） w1-r21-r22-r23：r21，r22和r23都读取 x=100，故 w2 = 1,000,000 （ii） r21-w1-r22-r23：r21读取 x=10，r22和r23读取，x=100，故 w2 = 100,000 （iii） r21-r22-w1-r23：r21和r22都读取 x=10，r23读取 x=100，故 w2 = 10,000 （iv） r21-r22-w1-r23：r21，r22和r23都读取 x=10，故 w2 = 1,000

综合以上结果，最后可能的值一共有5种：100，1,000，10,000，100,000，1,000,000
















































