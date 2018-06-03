#lang racket

(define (stream-null? s)
  (null? s))
(define the-empty-stream '())

;延迟求值

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))
(define-syntax-rule (delay x)
  (memo-proc (lambda() x)))

;这里使用宏代替函数
(define-syntax-rule (cons-stream a b)
  (cons a (delay b)))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))


(define (stream-enumerate-interval low high)
  (if(> low high)
     the-empty-stream
     (cons-stream low
                  (stream-enumerate-interval (+ 1 low) high))))
(define (stream-filter pred stream)
  (cond((stream-null? stream) the-empty-stream)
       ((pred (stream-car stream))
        (cons-stream (stream-car stream)
                     (stream-filter pred (stream-cdr stream))))
       (else (stream-filter pred (stream-cdr stream)))))


(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))


(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda(x)(* x factor))
              stream))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define (partial-sums stream)
  (cons-stream
   (stream-car stream)
   (add-streams (partial-sums stream)
                (stream-cdr stream))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))


(define (avergae a b)
  (/ (+ a b) 2.0))

(define (sqrt-improve guess x)
  (avergae guess (/ x guess)))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
                (stream-map - (pi-summands (+ 2 n)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))


(define (square x)(* x x))
;加速变换
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;流的流
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))
;取出上方每个表列的开头元素
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

#|
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda(guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

这个版本的 sqrt-stream 返回一个流 guesses 作为结果,当求值 (stream-ref guesses 1) 的时候,它直接返回定义中的 1.0 ;
当求值 (stream-ref guesses 2) 的时候,它对流进行强迫求值,并利用流的第一个值 1.0 来计算流的第二个值;
当求值 (stream-ref guesses 3) 的时候,它利用流的第二个值来计算第三个值,诸如此类,就这样一直做下去。
因为 memo-proc 的效果，对于每次计算 (stream-ref guesses n) ，流都可以直接返回前一个猜测，也即是 (stream guesses (- n 1)) 的值，因此，这个版本的复杂度为 Θ(n) 。
|#


(define (display-stream stream)
  (define (iter n s)
    (if(= 0 n)
       (display (stream-car s))
       (begin
         (display (stream-car s))
         (newline)
         (iter (- n 1) (stream-cdr s)))))
  (iter 10 stream))

;(dieplay-stream (sqrt-stream 2))

(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map
                    (lambda (guess)
                      (sqrt-improve guess x))
                    (sqrt-stream x))))

#|
这个 sqrt-stream 也返回一个流,但是它的执行方式和前面版本很不同:
当求值 (stream-ref (sqrt-stream x) 1) 的时候,它直接返回定义中的 1.0 ;当求值 (stream (sqrt-stream x) 2) 的时候,运行会产生这样的展开:
(cons-stream 1.0
             (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                         (sqrt-stream x)))
以上表达式的最后也是一个 (sqrt-stream x) 调用,因此,它会再一次执行 sqrt-stream :
(cons-stream 1.0
             (delay
                (stream-map (lambda (guess)
                                (sqrt-improve guess x))
                            (sqrt-stream x))))
组合起以上两个表达式,最终计算出 (stream-ref (sqrt-stream x) 2) :
(cons-stream 1.0
             (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                         (cons-stream 1.0
                                      (delay
                                          (stream-map (lambda (guess)
                                                          (sqrt-improve guess x))
                                                      (sqrt-stream x))))))
接着,当计算 (stream-ref (sqrt-stream x) 3) 的时候,运行会再一次展开,它先计算 (stream-ref (sqrt-stream x) 1) :
(cons-stream 1.0
             (delay
                (stream-map (lambda (guess)
                                (sqrt-improve guess x))
                            (sqrt-stream x))))
接着,计算 (stream-ref (sqrt-stream x) 2) :
(cons-stream 1.0
             (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                         (cons-stream 1.0
                                      (delay
                                          (stream-map (lambda (guess)
                                                          (sqrt-improve guess x))
                                                      (sqrt-stream x))))))
最后,计算 (stream-ref (sqrt-stream x) 3) :
(cons-stream 1.0
             (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                         (cons-stream 1.0
                                      (stream-map (lambda (guess)
                                                      (sqrt-improve guess x))
                                                  (cons-stream 1.0
                                                               (delay
                                                                   (stream-map (lambda (guess)
                                                                                   (sqrt-improve guess x))
                                                                               (sqrt-stream x))))))))
可以看到,每当对 (stream-ref (sqrt-stream x) n) 进行求值的时候,都要从 (stream-ref (sqrt-stream x) 1) 开始,一步一步地展开,直到展开到 (stream-ref (sqrt-stream x) n) 为止。

对于每个 (stream-ref (sqrt-stream x) n) ， sqrt-stream 都要计算 n 步，因此 Louis 的这个 sqrt-stream 的复杂度为 Θ(n2) 。

另外，需要注意的一点是，并不是这个版本的流就没有使用 memo-proc 的效果，并不是这样的

，只是当每次计算 (stream-ref (sqrt-stream x) n) 的时候，所有计算结果都会作为函数调用所产生的临时变量而消失，因此也就没办法真正地将 memo-proc 的效果使用上。

这也是为什么第一个版本的 sqrt-stream 要用 guesses 变量来持有流，而不是直接使用函数调用来产生流的原因。
|#



































