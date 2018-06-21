#lang sicp


;----------------------------------------------------------------------------------------------------------------------------------------------------------------

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

;------------------------------------------------------------------------------------------------------------------------------------------------------------

(define (make-table)
  (let([local-table (list '*table*)])
    (define (lookup key1 key2)
      (let([subtable
            (assoc key1 (cdr local-table))])
        (if subtable
            (let([record
                  (assoc key2 (cdr subtable))])
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key1 key2 value)
      (let([subtable (assoc key1 (cdr local-table))])
        (if subtable
            (let([record
                  (assoc key2 (cdr subtable))])
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key1 (cons key2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond((eq? m 'lookup-proc) lookup)
           ((eq? m 'insert-proc!) insert!)
           (else
            (error "Unknown operation: TABLE" m))))
    dispatch))


(define eval-apply-table (make-table))
(define put (eval-apply-table 'insert-proc!))
(define get (eval-apply-table 'lookup-proc))

;----------------------------------------------------------------------------------------------------------------------------------------------------------------


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



;----------------------------------------------------------------------------------------------------------------------------------------------------------------

#|
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
|#

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


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'op (car exp)) ((get 'op (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))


















