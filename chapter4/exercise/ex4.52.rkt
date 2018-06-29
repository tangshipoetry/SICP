#lang racket

;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)
;;; Starting a new problem
;;; Amb-Eval value:
all-odd
;;; Amb-Eval input:
(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)
;;; Starting a new problem
;;; Amb-Eval value:
8


;网上的
;; add this in analyze 
((if-fail? expr) (analyze-if-fail expr))
  
;; add those to amb evaluator 
(define (if-fail? expr) (tagged-list? expr 'if-fail))
  
(define (analyze-if-fail expr)
  (let ((first (analyze (cadr expr)))
        (second (analyze (caddr expr))))
    (lambda (env succeed fail)
      (first env
             (lambda (value fail2)
               (succeed value fail))
             (lambda ()
               (second env succeed fail))))))



































