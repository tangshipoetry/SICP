#lang racket


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((let? exp) (eval (let->combination exp) env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp)(and-squ-eval (and-content exp) env))
        ((or? exp) (or-squ-eval (or-content exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (make-lambda pro)
  (cons 'lambda pro))

;lambda表达式相关
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
;lambda表达式构造i函数
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


;抽象定义
(define (let? exp)
  (tagged-list? exp 'let))
(define (get-var varlist)
  (car varlist))
(define (get-val varlist)
  (cadr varlist))
(define (let->combination exp)
  (list (make-lambda (map car (cadr exp)) (caddr exp))
        (map cadr (cadr exp))))
































