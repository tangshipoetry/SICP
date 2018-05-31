#lang racket


假设银行按先来先服务(first come first service) 的方式处理业务,那么 Peter 、 Paul 和 Mary 的交易有以下六种可能的排列方式:

Peter -> Paul -> Mary
Peter -> Mary -> Paul
Paul -> Peter -> Mary
Paul -> Mary -> Peter
Mary -> Peter -> Paul
Mary -> Paul -> Peter
以上的这些排列方式会产生以下的余额值（括号里面表示的是执行操作之后的余额）：

Peter (110) -> Paul (90) -> Mary (45)
Peter (110) -> Mary (55) -> Paul (35)
Paul (80) -> Peter (90) -> Mary (45)
Paul (80) -> Mary (40) -> Peter (50)
Mary (50) -> Peter (60) -> Paul (40)
Mary (50) -> Paul (30) -> Peter (40)




Peter 、 Paul 和 Mary每个人分别对应一次读和写的操作,不妨记作:A1,A2(Peter),B1,B2(Paul),C1,C2(Mary),然后对它们进行排列,一共有6!种排法,但是“读”必须在对应的“写”前面,比如对Peter,A1必须在A2前,但在6!中排法中有一半正好是这样的,因此除以2,对Paul,Mary同样操作,故一共有6!/8=90种不同的排法,也就对应90种不同交错方式,但是需要注意的是结果并没有90种,有重复。枚举列出也不是那么麻烦

分情况

Peter最后完成写，按不同时刻读取有结果：110， 90， 60， 50， 40
Paul 最后完成写，按不同时刻读取有结果：80， 90， 30， 40，35
Mary 最后完成写，按不同时刻读取有结果：50，55，40，45
总结一下也就是 110，90，80，60，55，50，45，40，35，30 共10中不同结果































