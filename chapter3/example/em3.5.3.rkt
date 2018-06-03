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

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda(guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (square x)(* x x))
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


(define (pi-summands n)
  (cons-stream (/ 1.0 n)
                (stream-map - (pi-summands (+ 2 n)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
;(display-stream pi-stream)

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
(newline)
(display-stream
 (accelerated-sequence euler-transform pi-stream))
|#
(define (divisible? x y)
  (= (remainder x y) 0))
(define (prime? n)
  (define (iter guess)
    (cond ((> (square guess) n) true)
          ((divisible? n guess) false)
          (else (iter (+ 1 guess)))))
  (iter 2))


;序对无穷流
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
(stream-filter (lambda(pair)
                 (prime? (+ (car pair) (cadr pair))))
               (pairs integers integers))

































