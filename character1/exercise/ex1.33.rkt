#lang racket

#|
;iterative
(define (accumulate combiner null-value term a next b)
  (define (accu-iter result a b)
    (if(> a b)
       result
       (accu-iter (combiner (term a) result)
                  (next a)
                  b)))
  (accu-iter null-value a b))
|#

;判断奇偶
(define (even? x)
  (= 0 (remainder x 2)))
;求平方
(define (square x)
  (* x x))

;抽象accumulate过程
;recursive
(define (accumulate combiner null-value term a next b)
  (if(> a b)
     null-value
     (combiner (term a)
               (accumulate combiner null-value term (next a) next b))))

;过滤组合
(define (filtered-accumulate combiner null-value term a next b p)
  (cond((> a b) null-value)
       ((p a) (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b p)))
       (else (filtered-accumulate combiner null-value term (next a) next b p))))


#|base的exp次幂对m取模|#
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((nontrivial-root? base m) 0)
        ((even? exp) (remainder (square(expmod base
                                               (/ exp 2)
                                               m))
                                m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;Miller-Rabin-test
(define (MR-test n)
  (define a (+ 1 (random (- n 1))))
  (= 1 (expmod a (- n 1) n)))

;非凡平方根检测
(define (nontrivial-root? a n)
  (and
   (not (= a 1))
   (not (= a (- n 1)))
   (= 1 (remainder (square a) n))))
;Miller-Rabin检查素数
(define (prime? n)
  (let((times (ceiling (/ n 2))))
    (MR-iter n times)))

(define (MR-iter n times)
  (cond ((= times 0) #t)
        ((MR-test n) (MR-iter n (- times 1)))
        (else #f)))


;确定最小公约数
(define (gcd a b)
  (if(= b 0)
     a
     (gcd b
          (remainder a
                     b))))

(define (f x) x)

(define (inc x)(+ 1 x))

;计算素数之和
(define (sum-prime a b)
  (filtered-accumulate + 0 f a inc b prime?))


;计算小于n且与n互质数之积
(define (factorial-prime n)
  (define (prime-n? x)
    (= 1 (gcd n x)))
  (filtered-accumulate * 1 f 1 inc n prime-n?))


















