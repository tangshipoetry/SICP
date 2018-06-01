#lang racket

;来自网上，有疑问
(serialized-exchange peter-acc mary-acc)

(let ((serializer1 (peter-acc 'serializer))     ; 分别用 peter-acc 和 mary-acc 的 balance-serializer
      (serializer2 (mary-acc 'serializer)))     ; 串行化 exchange
    ((serializer1 (serializer2 exchange))
     peter-acc
     mary-acc))

(exchange peter-acc mary-acc)

(let ((difference (- (peter-acc 'balance)
                     (mary-acc 'balance))))
    ((peter-acc 'withdraw) difference)
    ((mary-acc 'deposit) difference))

((peter-acc 'withdraw) difference)

((balance-serializer withdraw) difference)      ; 噢, peter-acc 试图再次使用 balance-serializer 进行串行化



注意,在代码的最后一步, peter-acc 试图再次使用 balance-serializer 对 withdraw 操作进行串行化,但是在调用 exchange 的时候, balance-serializer 已经被调用过了,如果 withdraw 操作也使用 balance-serializer 进行串行化,那么它们就处在同一个串行化组中,这时运行 withdraw 和 exchange 的两个过程都会被阻塞,产生死锁。








The correct implementation of serialized-exchange in the book deliberately leaves the dispatched withdraw and deposit procedures *un*-serialized,
so that when the "raw" exchange procedure calls them and then gets wrapped, via serialized-exchange, in the serializers of both accounts involved in the exchange,
there will be no conflict.

Louis' proposed change would be disastrous because the dispatched withdraw and deposit procedures called in the "raw" exchange procedure would *already* have serializers,
so when serialized-exchange wraps the raw exchange in both serializers *again*, it wouldn't even be possible to perform the required withdraws and deposits, *by definition*,
since two procedures can be run concurrently if and only if they have *not* been serialized with the same serializer.
















