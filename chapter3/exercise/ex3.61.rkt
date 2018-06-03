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


(define (reciprocal x)
  (/ 1 x))
(define reciprocal-integers
  (stream-map reciprocal integers))

(define (integrate-series stream)
  (stream-map * stream reciprocal-integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))


(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))


(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

;自己写的
(define (invertunit-series stream)
  (cons-stream (stream-car stream)
               (scale-stream
                (mul-series (stream-cdr stream)
                            (invertunit-series stream))
                -1)))

;摘抄网上
(define (reciprocal-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (reciprocal-series s)) -1)))























