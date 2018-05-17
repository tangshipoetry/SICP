#lang racket

;过滤器
(define (filter p sequence)
  (if(null? sequence)
     '()
     (if(p (car sequence))
        (cons (car sequence)
              (filter p (cdr sequence)))
        (filter p (cdr sequence)))))

;interval enumerate
(define (enumerate-interval lower high)
  (if(> lower high)
     null
     (cons lower
           (enumerate-interval (+ 1 lower)
                               high))))

;操作累计
(define (accumulate op initial sequence)
  (if(null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))

;列表中元素之和
(define (sum-list s)
  (accumulate +
              0
              s))

;判断列表中元素之和是否与某个数相等
(define (equal-sum-n? s n)
  (= (sum-list s) n))

(define (flatmap proc seq)
  (accumulate append
              null
              (map proc seq)))

;生成某个数的三元组————自己写的
(define (triple n)
  (flatmap (lambda(i)
             (flatmap (lambda(j)
                        (map (lambda(k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triple-sum-equal-s n s)
  (filter (lambda(seq)
            (equal-sum-n? seq s))
          (triple n)))


(define (unique-pairs n)
  (flatmap (lambda(i)
             (map (lambda(j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


;网上抄的
(define (unique-triples n)
    (flatmap (lambda (i)
                 (map (lambda (j)                   ; cons 起 i 元素和二元组 j ,组成三元组
                          (cons i j))
                      (unique-pairs (- i 1))))      ; 生成不大于 i 的所有相异整数二元组
             (enumerate-interval 1 n)))             ; 生成 1 至 n 的所有整数，作为 i 







































