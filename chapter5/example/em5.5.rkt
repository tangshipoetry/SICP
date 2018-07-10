#lang racket

read-eval-print-loop
(perform (op initialize-stack))
(perform
 (op prompt-for-input) (const ";;EC-Eval input:"))
(assign exp (op read))
(assign env (op get-global-environment))
(assign continue (label print-result))
(goto (label eval-dispatch))
print-result
(perform (op announce-output) (const ";;EC-Eval value:"))
(perform (op user-print) (reg val))
(goto (label read-eval-print-loop))


;遇到错误时打印错误
unknown-expression-type
(assign val (const unknown-expression-type-error))
(goto (label signal-error))
unknown-procedure-type
(restore continue) ; clean up stack (from apply-dispatch)
(assign val (const unknown-procedure-type-error))
(goto (label signal-error))
signal-error
(perform (op user-print) (reg val))
(goto (label read-eval-print-loop))


;模拟机器
(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
   '(read-eval-print-loop
     ⟨entire machine controller as given above⟩ )))


;定义scheme过程
(define eceval-operations
  (list (list 'self-evaluating? self-evaluating)
        ⟨complete list of operations for eceval machine⟩))



















