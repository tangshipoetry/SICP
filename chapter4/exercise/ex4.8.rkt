#lang racket

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((let? exp) (eval (let->combination exp) env))
        
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


#|
;网上的
;判断
(define (named-let? expr) (and (let? expr) (symbol? (cadr expr))))
;获取函数名
(define (named-let-func-name expr) (cadr expr))
;获取函数体
(define (named-let-func-body expr) (cadddr expr))
;获取函数参数名列表
(define (named-let-func-parameters expr) (map car (caddr expr))) 
;获取函数参数值列表
(define (named-let-func-inits expr) (map cadr (caddr expr))) 
;
(define (named-let->func expr) 
  (list 'define  
        (cons (named-let-func-name expr) (named-let-func-parameters expr))
        (named-let-func-body expr)))
;
(define (let->combination expr) 
  (if (named-let? expr) 
      (sequence->exp 
       (list (named-let->func expr) 
             (cons (named-let-func-name expr) (named-let-func-inits expr)))) 
      (cons (make-lambda (let-vars expr) 
                         (list (let-body expr))) 
            (let-inits expr))))
|#




;自己仿照写的
;命名let
(define (has-name? exp)
  (symbol? (cadr exp)))
(define (name-let-func-name exp)
  (cadr exp))
(define (name-let-func-body exp)
  (cadddr exp))
(define (name-let-parameters exp)
  (map car (caddr exp)))
(define (name-let-inits exp)
  (map cadr (caddr exp)))
(define (name-let->proc exp)
  (list 'define
        (name-let-func-name exp)
        (make-lambda (name-let-parameters exp)
                     (name-let-func-body exp))))
;抽象定义let
(define (let? exp)
  (tagged-list? exp 'let))
(define (get-var varlist)
  (car varlist))
(define (get-val varlist)
  (cadr varlist))
(define (let->combination exp)
  (if(has-name? exp)
     (sequence->exp
      (list (name-let->proc exp)
            (list (name-let-func-name exp) (name-let-inits exp))))
     (list (make-lambda
            (map car (cadr exp))
            (caddr exp))
           (map cadr (cadr exp)))))


;另一个网上的参考
(define (let? exp) (tagged-list? exp 'let)) 
(define (let-has-name? exp) (symbol? (cadr exp)))
(define (let-name exp) (cadr exp))
(define (let-vardefs exp)
  (if (let-has-name? exp)
      (caddr exp)
      (cadr exp)))
(define (let-body exp) 
  (if (let-has-name? exp) 
      (cdddr exp) 
      (cddr exp)))
(define (let->combination exp) 
  (let ((res (fold-right 
              (lambda (new rem) 
                (cons (cons (car new) (car rem)) 
                      (cons (cadr new) (cdr rem)))) 
              (cons '() '()) 
              (let-vardefs exp)))) 
    (let ((vars (car res)) 
          (vexps (cdr res))) 
      (define proc (make-lambda vars (let-body exp))) 
      (if (let-has-name? exp) 
          ;;create a lambda with no args containing: 
          ;;(i) definition of the actual lambda(proc) 
          ;;(ii) invocation of proc with supplied expressions. 
          ;;finally create application for this no argument lambda. 
          (cons 
           (make-lambda '() 
                        (list (list 'define (let-name exp) proc) 
                              (cons (let-name exp) vexps) 
                              )) 
           '()) 
          (cons proc vexps) 
          )))) 










