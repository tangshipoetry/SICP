#lang racket

;原版
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

;网上的——没看
;; remove the condition, when (preserving regs, seq1 seq2), always (save first-reg),  
;; then (restore first-reg) 
(define (preserving regs seq1 seq2) 
  (if (null? regs) 
      (append-instruction-sequences seq1 seq2) 
      (let ((first-reg (car regs))) 
        (preserving (cdr regs) 
                    (make-instruction-sequence 
                     (list-union (list first-reg) (registers-needed seq1)) 
                     (list-difference 
                      (registers-modified seq1) 
                      (list first-reg)) 
                     (append 
                      `((save ,first-reg)) 
                      (statements seq1) 
                      `((restore ,first-reg)))) 
                    seq2)))) 
  
;; compare the following code with exercise 5.35 
(continue env)              
(val) 
(save continue) 
(save env) 
(save continue) 
(assign val (op make-compiled-procedure) (label entry1) (reg env)) 
(restore continue) 
(goto (label after-lambda2)) 
entry1 
(assign env (op compiled-procedure-env) (reg proc)) 
(assign env (op extend-environment) (const (x)) (reg argl) (reg env)) 
(save continue) 
(save env) 
(save continue) 
(assign proc (op lookup-variable-value) (const +) (reg env)) 
(restore continue) 
(restore env) 
(restore continue) 
(save continue) 
(save proc) 
(save env) 
(save continue) 
(save env) 
(save continue) 
(assign proc (op lookup-variable-value) (const g) (reg env)) 
(restore continue) 
(restore env) 
(restore continue) 
(save continue) 
(save proc) 
(save continue) 
(save env) 
(save continue) 
(assign proc (op lookup-variable-value) (const +) (reg env)) 
(restore continue) 
(restore env) 
(restore continue) 
(save continue) 
(save proc) 
(save env) 
(save continue) 
(assign val (const 2)) 
(restore continue) 
(assign argl (op list) (reg val)) 
(restore env) 
(save argl) 
(save continue) 
(assign val (op lookup-variable-value) (const x) (reg env)) 
(restore continue) 
(restore argl) 
(assign argl (op cons) (reg val) (reg argl)) 
(restore proc) 
(restore continue) 
(test (op primitive-procedure?) (reg proc)) 
(branch (label primitive-branch3)) 
compiled-branch4 
(assign continue (label after-call5)) 
(assign val (op compiled-procedure-entry) (reg proc)) 
(goto (reg val)) 
primitive-branch3 
(save continue) 
(assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
(restore continue) 
after-call5 
(assign argl (op list) (reg val)) 
(restore proc) 
(restore continue) 
(test (op primitive-procedure?) (reg proc)) 
(branch (label primitive-branch6)) 
compiled-branch7 
(assign continue (label after-call8)) 
(assign val (op compiled-procedure-entry) (reg proc)) 
(goto (reg val)) 
primitive-branch6 
(save continue) 
(assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
(restore continue) 
after-call8 
(assign argl (op list) (reg val)) 
(restore env) 
(save argl) 
(save continue) 
(assign val (op lookup-variable-value) (const x) (reg env)) 
(restore continue) 
(restore argl) 
(assign argl (op cons) (reg val) (reg argl)) 
(restore proc) 
(restore continue) 
(test (op primitive-procedure?) (reg proc)) 
(branch (label primitive-branch9)) 
compiled-branch10 
(assign val (op compiled-procedure-entry) (reg proc)) 
(goto (reg val)) 
primitive-branch9 
(save continue) 
(assign val (op apply-primitive-procedure) (reg proc) (reg argl)) 
(restore continue) 
(goto (reg continue)) 
after-call11 
after-lambda2 
(restore env) 
(perform (op define-variable!) (const f) (reg val) (reg env)) 
(assign val (const ok)) 
(restore continue) 



















































