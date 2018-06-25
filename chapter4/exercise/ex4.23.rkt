#lang racket


(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))


(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))



(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))




(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))



(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))


;题目中的
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else
           ((car procs) env)
           (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (lambda (env)
      (execute-sequence procs env))))



(loop first rest)
 
(loop (s first (car rest)) (cdr rest))

(loop (s (s first (car rest)) (cadr rest))
      (cddr rest))


#|
网上的
 In Alyssa's analyze-sequence, execute-sequence is running in runtime. But the solution in the text unrolls this procedure.  for example: for  ((lambda (x) (+ x 1)) 1), 
 Alyssa's analyze-sequence is (lambda (env) (execute-sequence (lambda ...) env)), 
 analyze-sequence in text is (lambda (env) ((lambda ...) env)). 
|#





;Alyssa 版本的分析过程直接返回了(lambda (env) (execute-sequence procs env)),在运行时才去展开execute-sequence,而正文给出的版本在分析时就把procs给展开了。

;举个例子，假设序列中只有一个表达式E1，解析后的表达式为analyzed-E1，两个版本analyze-sequence分析后的结果是：

; 正文给出的版本的结果
(lambda (env) (analyzed-E1 env))
; Alyssa 给出的版本的结果
(lambda (env) (execute-sequence (analyzed-E1)))


;一个的话不是特别明显，我们可以假设序列中有四个表达式：

; 正文给出的版本的结果
(lambda (env)
  ((lambda (env)
     ((lambda (env)
        (proc1 env)
        (proc2 env))
      env)
     (proc3 env))
   env)
  (proc4 env))
; Alyssa 给出的版本的结果
(lambda (env)
  (execute-sequence (proc1 proc2 proc3 proc4) env)) 
































