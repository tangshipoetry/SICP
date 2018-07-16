#lang racket

;网上的


(assign val (op make-compiled-procedure) (label entry24) (reg env))
(goto (label after-lambda23))
  
entry24
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;;生成(define (iter product counter) ...)
(assign val (op make-compiled-procedure) (label entry29) (reg env))
(goto (label after-lambda28))

entry29 
;;设置环境啦
(assign env (op compiled-procedure-env) (reg proc))
(assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
;;计算(> counter n) 
(save continue)
(save env)
(assign proc (op lookup-variable-value) (const >) (reg env))
(assign val (op lookup-variable-value) (const n) (reg env))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const counter) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch44))
  
compiled-branch43
(assign continue (label after-call42))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))

primitive-branch44
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))

after-call42
;;虽然(> counter n)并未改变env。但过程应用的modified是all-regs,下面紧接着是另一个过程应用,need env,所以会产生这个步骤
(restore env)
(restore continue)
(test (op false?) (reg val))
(branch (label false-branch31))
  
true-branch32
(assign val (op lookup-variable-value) (const product) (reg env))
(goto (reg continue))
  
false-branch31
(assign proc (op lookup-variable-value) (const iter) (reg env))
;;计算(+ counter 1)，不会马上给回counter，不用担心product
(save continue)
(save proc)
(save env)
(assign proc (op lookup-variable-value) (const +) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const counter) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch38))

compiled-branch37
(assign continue (label after-call36))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))

primitive-branch38
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))

after-call36
(assign argl (op list) (reg val))
;;计算(* product counter)
(restore env)
(save argl)
(assign proc (op lookup-variable-value) (const *) (reg env))
(assign val (op lookup-variable-value) (const counter) (reg env))
(assign argl (op list) (reg val))
(assign val (op lookup-variable-value) (const product) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch35))
  
compiled-branch34
(assign continue (label after-call33))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
  
primitive-branch35 
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  
after-call33
(restore argl)
;;给iter新的argl构建完毕
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(restore continue)
;;至此，栈又恢复到上次调用iter的情况
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch41))

compiled-branch40
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val)) 

primitive-branch41
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))
  
;;用不上了 
after-call39 
after-if30 
  
;;得到(define (iter ..) ...)
after-lambda28
(perform (op define-variable!) (const iter) (reg val) (reg env))
(assign val (const ok))
;;计算(iter 1 1)
(assign proc (op lookup-variable-value) (const iter) (reg env))
;;构建argl
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val (const 1))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch27))
  
;;最后一条表达式，所以不用回到after-call25 
compiled-branch26
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
  
primitive-branch27
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
(goto (reg continue))

after-call25

after-lambda23
(perform (op define-variable!) (const factorial) (reg val) (reg env))
(assign val (const ok))

























































