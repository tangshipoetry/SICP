#lang racket

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
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



;抽象定义let
(define (let? exp)
  (tagged-list? exp 'let))
(define (get-var varlist)
  (car varlist))
(define (get-val varlist)
  (cadr varlist))
(define (let->combination exp)
  (list (make-lambda (map car (cadr exp)) (caddr exp))
        (map cadr (cadr exp))))

;抽象定义let*
(define (make-let binds body)
  (list 'let binds body))
(define (let*-bindings exp)(cadr exp))
(define (let*-body exp)(caddr exp))
(define (let*->nested-lets exp)
  (expand-let*->let (let*-bindings exp) (let*-body exp)))
(define (last-let*-bind binds)
  (null? (cdr binds)))
(define (expand-let*->let binds body)
  (if(null? binds)
     (make-let binds body)
     (if(last-let*-bind binds)
        (make-let binds body);参数列表保持列表状态
        (make-let (list (car binds)) (expand-let*->let (cdr binds) body)))))






;网上的
;; let* expression 
(define (let*? expr) (tagged-list? expr 'let*)) 
(define (let*-body expr) (caddr expr)) 
(define (let*-inits expr) (cadr expr)) 
(define (let*->nested-lets expr) 
  (let ((inits (let*-inits expr)) 
        (body (let*-body expr))) 
    (define (make-lets exprs) 
      (if (null? exprs) 
          body 
          (list 'let (list (car exprs)) (make-lets (cdr exprs))))) 
    (make-lets inits)))






























