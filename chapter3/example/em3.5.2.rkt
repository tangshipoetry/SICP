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


;正整数流
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y)
  (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda(x)(not (divisible? x 7)))
                 integers))

;斐波那契流
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

;厄拉多塞筛法
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda(x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))



;隐式流

(define ones
  (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (scale-stream stream factor)
  (stream-map (lambda(x)(* x factor))
              stream))
;整数
(define integers-by-ones
  (cons-stream 0 (add-streams ones integers-by-ones)))
;斐波那契
(define fib
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fib)
                                         fib))))

(define double (cons-stream 1 (scale-stream double 2)))

(define (square x)
  (* x x))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter prime))

(define prime
  (cons-stream
   2
   (stream-filter prime?
                  (integers-starting-from 3))))







