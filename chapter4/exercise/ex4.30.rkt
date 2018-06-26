#lang racket


(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))




;以下都来自网上

; Cy 给出的版本
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; 正文给出的版本
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))




;a)
(define (for-each proc items)
  (if (null? items)
    'done
    (begin (proc (car items))
           (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 5 6 7 8))
;正文版的 eval-sequence 之所以能够正确处理 for-each,是因为for-each中不需要关心的proc的返回值





;b)
(define (p1 x)
  (set! x (cons x '(2))))

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))  



	(p1 1)	(p2 1)
Cy的版本	(1 . 2)	(1 . 2)
正文的版本(1 . 2) 1


;c)
;如 a) 中所说,Cy 版本的程序只是强加了一层限制,会去求传入eval-sequence中每一个表达式的值而已




#|
;; a 
In begin expression, every expression will be evaluated using eval, and display is primitive function, it will call force-it to get x. 
  
;; b 
original eval-sequence: 
(p1 1) => (1 . 2) 
(p2 1) => 1  . because (set! x (cons x '(2))) will be delayed, in function p, when evaluating it, it's a thunk. 
  
Cy's eval-sequence: 
(p1 1) => (1 . 2) 
(p2 1) => (1 . 2). thunk (set! x (cons x '(2))) will be forced to evaluate. 
  
;; c 
when using actual-value, it will call (force-it p), if p is a normal value, force-it will return p, just as never call actual-value 
|#













