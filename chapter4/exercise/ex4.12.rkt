#lang racket


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
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))


(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)




;表达式的表示
;判断和组成获取
;确定一个表开头是不是给定符号
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;自求值
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
;变量
(define (variable? exp) (symbol? exp))
;引号表达式
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
;赋值
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
;定义的形式
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body
;lambda表达式相关
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
;lambda表达式构造i函数
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
;条件
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
;条件表达式构造
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;序列表达式构造
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))
;过程应用表达式
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;谓词检测
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))


;复合过程构造
(define (make-procedure parameters body env)
(list 'procedure parameters body env))
(define (compound-procedure? p)
(tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;对环境的操作
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
;绑定变量名值,制作框架
(define (make-frame variables values)
  (if(= (length variables) (length values))
     (if(null? variables)
        '()
        (cons (cons (car variables) (car values))
              (make-frame (cdr variables) (cdr values))))
     (error "variables and values have different lengthes" variables values)))


;(define (frame-variables frame) (car frame))
;(define (frame-values frame) (cdr frame))
;将原框架中不存在的名值绑定并放到框架中
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons (cons var val) frame)))
;新框架扩充环境
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
;查找变量
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vas)
      (cond ((null? vas)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar vas)) (cdar vas))
            (else (scan (cdr vas)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

;修改已有变量值
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vas)
      (cond ((null? vas)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar vas)) (set-cdr! (car vas) val))
            (else (scan (cdr vas)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vas)
      (cond ((null? vas)
             (add-binding-to-frame! var val frame))
            ((eq? var (caar vars)) (set-cdr! (car vas) val))
            (else (scan (cdr vas)))))
    (scan frame)))

;自己写的,基于4.11练习
(define (search-val var env)
  (let((frame (first-frame env)))
    (define (scan vas)
      (cond ((null? vas) (search-val var (enclosing-environment env)))
            ((eq? var (caar vars)) (car vas))
            (else (scan (cdr vas)))))
    (scan frame)))

;变量定义改写
(define (define-variable! var val env)
  (let([temp-env (list (first-frame env))])
    (let((result (search-val var env)))
      (if(null? result)
         (add-binding-to-frame! var val (first-frame env))
         (set-cdr! result val)))))
;变量修改改写
(define (set-variable-value! var val env)
  (set-cdr! (search-val var env) val))

;查询
(define (lookup-variable-value var env)
  (cdr (search-val var env)))




;网上的
(define (env-loop env base match) 
  (let ((frame (first-frame env))) 
    (define (scan vars vals) 
      (cond ((null? vars) 
             base) 
            ((eq? var (car vars)) 
             match)                  
            (else (scan (cdr vars) (cdr vals))))) 
    (scan (frame-variables frame) 
          (frame-values frame)))) 
  
(define (lookup-variable-value var env) 
  (env-loop env 
            (env-loop (enclosing-environment env)) 
            (car vals))) 
  
(define (set-variable-value! var val env) 
  (env-loop env 
            (env-loop (enclosing-environment env)) 
            (set-car! vals val))) 
  
(define (define-variable! var val env) 
  (env-loop env 
            (add-binding-to-frame! var val frame) 
            (set-car! vals val))) 


;网上的
;; general procedure 
(define (env-loop match-proc end-frame end-env env)
  (define (scan vars vals current-frame current-env) 
    (cond ((null? vars) 
           (end-frame current-frame current-env)) 
          ((eq? var (car vars)) 
           (match-proc vars vals current-frame current-env)) 
          (else 
           (scan (cdr vars) (cdr vals) current-frame current-env)))) 
  (if (eq? env the-empty-environment) 
      (end-env) 
      (let ((frame (first-frame env))) 
        (scan (frame-variables frame) 
              (frame-values frame) 
              frame env)))) 
  
;; lookup-variable-value 
(define (lookup-variable-value var env) 
  (define (match-proc vars vals cur-frame cur-env) (car vals)) 
  (define (end-env) (error "Unbound variable" var)) 
  (define (end-frame cur-frame cur-env)                       ;; !!! 
    (env-loop match-proc end-frame end-env (enclosing-environment cur-env))) 
  (env-loop match-proc end-frame end-env env)) 
  
;; set-variable-value! 
(define (set-variable-value! var val env) 
  (define (match-proc vars vals cur-frame cur-env) (set-car! vals val)) 
  (define (end-env) (error "Unbound variable" var)) 
  (define (end-frame cur-frame cur-env)                       ;; !!! 
    (env-loop match-proc end-frame end-env (enclosing-environment cur-env))) 
  (env-loop match-proc end-frame end-env env)) 
  
;; define-variable! 
(define (define-variable! var val env) 
  (define (match-proc vars vals cur-frame cur-env) (set-car! vals val)) 
  (define (end-env) (error "Unbound variable" var)) 
  (define (end-frame cur-frame cur-env)                       ;; !!! 
    (add-binding-to-frame! var val cur-frame)) 
  (env-loop match-proc end-frame end-env env)) 




















