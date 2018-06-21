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
(put 'op 'and and-eval)
(put 'op 'or or-eval)
(define (and-squ-eval s env)
  (if(null? s)
     #t
     (if(eval (car s) env)
        (and-squ-eval (cdr s) env)
        #f)))
(define (or-squ-eval s env)
  (if(null? s)
     #f
     (if(eval (car s) env)
        #t
        (or-squ-eval (cdr s) env))))

(define (and-eval exp env)
  (and-squ-eval (and-content exp) env))
(define (or-eval exp env)
  (or-squ-eval (or-content exp) env))


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
        ((and? exp)(and-squ-eval (and-content exp) env))
        ((or? exp) (or-squ-eval (or-content exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))
|#



;派生
(define (and-clause exp)
  (cdr exp))
(define (or-clause exp)
  (cdr exp))

(define (and->if exp)
  (and-expand (and-clause exp)))

(define (and-expand clauses)
  (if(null? clauses)
     #t
     (let([first (car clauses)]
          [rest (cdr clauses)])
       (if(null? rest)
          (sequence->exp first)
          (make-if first
                   (and-expand rest)
                   #f)))))

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






























