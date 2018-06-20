#lang racket

#|
(define (eval exp env)
  (cond 
        ((variable? exp) (lookup-variable-value exp env))

        
        
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
       
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))
|#




#|
application?判断依据是pair?，define表达式会进入其中。
之后会求值符号define,在环境中没有结果
|#

;摘抄网上
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))


(define (lookup-variable-value exp env)
  '())
(put 'eval 'variable lookup-variable-value)
(define (eval-definition exp env)
  '())
(put 'eval 'define eval-definition)
(define (eval-definition exp env)
  '())
(put 'eval 'define eval-definition)
(define (eval-assign exp env)
  '())
(put 'eval 'define eval-definition)




(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((quoted? exp) (text-of-quotation exp))
        ((cond? exp) (eval (cond->if exp) env))))



























