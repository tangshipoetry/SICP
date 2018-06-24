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



;(a)
;查找变量
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if(eq? (car vals) *unassigned*)
                (error "*unassigned*" var)
                (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


;(b)

(define inter-define(body)
  (map (lambda(proc)
         (cond((eq? (car proc) 'define) proc)))
       body))

(define (inter-unassign definitions)
  (map (lambda(definition)(list (cadr definition) *unassigned*))
       definitions))

(define (inter-assign definitions)
  (map (lambda(definition)(list 'set! (cadr definition) (caddr definition)))
       definitions))

(define non-inter-define(body)
  (map (lambda(proc)
         (cond((not (eq? (car proc) 'define)) proc)))
       body))

(define (scan-out-defines body)
  (let ((definitions (inter-define body)))
    (let([vars-init (inter-unassign definitions)]
         [vars-assign (inter-assign definitions)])
      (list 'let vars-init (make-begin (append vars-assign (non-inter-define body)))))))






;网上的
;; a, change look-up-variable-value 
(define (lookup-variable-value var env) 
  (define (env-lookup env) 
    (define (scan vars vals) 
      (cond ((null? vars) (env-lookup (enclosing-environment env))) 
            ((eq? var (car vars))  
             (if (eq? (car vals) '*unassigned*) 
                 (error "variable is unassigned" var) 
                 (car vals))) 
            (else (scan (cdr vars) (cdr vals))))) 
    (if (eq? env the-empty-environment) 
        (error "Unbound variable" var) 
        (let ((frame (first-frame env))) 
          (scan (frame-variables frame) 
                (frame-values frame))))) 
  (env-lookup env)) 
  
  
;; b 
(define (scan-out-defines body)
  (define (name-unassigned defines)
    (map (lambda (x) (list (definition-variable x) '*unassigned*)) defines))
  (define (set-values defines)
    (map (lambda (x)
           (list 'set! (definition-variable x) (definition-value x)))
         defines))
  (define (defines->let exprs defines not-defines)
    (cond ((null? exprs)
           (if (null? defines)
               body
               (list (list 'let (name-unassigned defines)
                           (make-begin (append (set-values defines)
                                               (reverse not-defines)))))))
          ((definition?(car exprs)) 
           (defines->let (cdr exprs) (cons (car exprs) defines) not-defines))
          (else (defines->let (cdr exprs) defines (cons (car exprs) not-defines)))))
  (defines->let body '() '()))


;install scan-out-defines into make-procedure. otherwise, when we call procedure-body, procedure scan-out-defines will be called.














