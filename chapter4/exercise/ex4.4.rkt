#lang racket

(put 'op 'quote (lambda(exp env)(text-of-quotation exp)))
(put 'op 'set! eval-assignment)
(put 'op 'define eval-definition)
(put 'op 'if (lambda(env exp) (eval-if exp env)))
(put 'op 'lambda (lambda(exp env)
                   (make-procedure (lambda-parameters exp)
                                   (lambda-body exp)
                                   env)))
(put 'op 'begin (lambda(exp env)(eval-sequence (begin-actions exp) env)))
(put 'op 'cond (lambda(exp env)(eval (cond->if exp) env)))


;数据导向
(put 'op 'and eval-and)
(put 'op 'or eval-or)



(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'op (car exp)) ((get 'op (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))


#|
;直接写入
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp)(eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))
|#


;and抽象定义
(define (last-and? exp)
  (null? (cdr exp)))
(define (first-and exp)
  (car exp))
(define (rest-and exp)
  (cdr exp))
(define (eval-and exp env)
  (eval-and-squ (cdr exp) env))
(define (eval-and-squ squ env)
  (cond((last-and? squ)(eval (first-and squ) env))
       ((eval (car squ) env) (eval-and-squ (rest-and squ) env))
       (else #f)))

;or抽象定义
(define (last-or? exp)
  (null? (cdr exp)))
(define (first-or exp)
  (car exp))
(define (rest-or exp)
  (cdr exp))
(define (eval-or exp env)
  (eval-or-squ (cdr exp) env))
(define (eval-or-squ squ env)
  (cond((last-or? squ)(eval (first-or squ) env))
       ((eval (car squ) env) #t)
       (else (eval-or-squ (rest-or squ) env))))



;派生
(define (and? exp)(tagged-list? exp 'and))
(define (and-exps exps)(cdr exps))
(define (last-exp? exps)(null? (cdr exps)))
(define (and->if exps)
  (and-expand (and-exps exps)))
(define (and-expand exps)
  (if(null? exps)
     'true
     (let([first (first-and exps)]
          [rest (rest-and exps)])
       (make-if first
                (and-expand rest)
                'false))))



(define (or-expand clauses)
  (if(null? clauses)
     #f
     (let([first (car clauses)]
          [rest (cdr clauses)])
       (if(null? rest)
          (sequence->exp first)
          (make-if first
                   #t
                   (or-expand rest))))))






























