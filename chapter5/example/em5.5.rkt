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


;compile过程
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence
          (begin-actions exp) target linkage))
        ((cond? exp)
         (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type: COMPILE" exp))))



;指令序列关联寄存器使用信息的表实现形式
(define (make-instruction-sequence
         needs modifies statements)
  (list needs modifies statements))


;生成指令实现linkage
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
                                    `((goto (label ,linkage)))))))



(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))



;简单表达式的编译
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '() (list target)
                                               `((assign ,target (const ,exp))))))
(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '() (list target)
                                               `((assign ,target (const ,(text-of-quotation exp)))))))
(define (compile-variable exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence '(env) (list target)
                                               `((assign ,target
                                                         (op lookup-variable-value)
                                                         (const ,exp)
                                                         (reg env))))))



;定义和赋值
(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op set-variable-value!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))
(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence '(env val) (list target)
                                                             `((perform (op define-variable!)
                                                                        (const ,var)
                                                                        (reg val)
                                                                        (reg env))
                                                               (assign ,target (const ok))))))))




;条件表达式的编译
;形式
⟨compilation of predicate, target val, linkage next⟩
(test (op false?) (reg val))
(branch (label false-branch))
true-branch
⟨compilation of consequent with given target
and given linkage or after-if⟩
false-branch
⟨compilation of alternative with given target and linkage⟩
after-if

;if内部分支标记
(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))


(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
             (compile
              (if-consequent exp) target
              consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence '(val) '()
                                                `((test (op false?) (reg val))
                                                  (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))


;表达式序列的编译
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving
       '(env continue)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))


;lambda表达式编译
;辅助程序
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
                          (make-instruction-sequence '(env) (list target)
                                                     `((assign ,target
                                                               (op make-compiled-procedure)
                                                               (label ,proc-entry)
                                                               (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))


(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
                                `(,proc-entry
                                  (assign env
                                          (op compiled-procedure-env)
                                          (reg proc))
                                  (assign env
                                          (op extend-environment)
                                          (const ,formals)
                                          (reg argl)
                                          (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))




;组合式的编译
(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda
                  (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))
;构造实参列表指令
(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
                                   '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                                           '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args
                           (cdr operand-codes))))))))
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence '(val argl) '(argl)
                                                '((assign argl
                                                          (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))





;过程应用
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
                           (make-instruction-sequence '(proc argl)
                                                      (list target)
                                                      `((assign ,target
                                                                (op apply-primitive-procedure)
                                                                (reg proc)
                                                                (reg argl)))))))
       after-call))))



;连接是一个label的形式
(assign continue (label proc-return))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
proc-return
(assign ⟨target⟩ (reg val)) ; included if target is not val
(goto (label ⟨linkage⟩)) ; linkage code
;连接是一个return的形式
(save continue)
(assign continue (label proc-return))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
proc-return
(assign ⟨target⟩ (reg val)) ; included if target is not val
(restore continue)
(goto (reg continue)) ; linkage code

;生成过程调用代码
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (assign val (op compiled-procedure-entry)
                                              (reg proc))
                                      (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (assign val (op compiled-procedure-entry)
                                                (reg proc))
                                        (goto (reg val))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue)
          all-regs
          '((assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (eq? linkage 'return))
         (error "return linkage,target not val: COMPILE"
                target))))




(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))


(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))


;连接任意多顺序执行指令序列
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union
      (registers-needed seq1)
      (list-difference (registers-needed seq2)
                       (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences
         (car seqs)
         (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))



(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))
(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

;seq1与seq2结合,加上seq1前后合适的save和restore操作
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

;用于compile-lambda中,将过程体和另一个序列结合
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
           (statements body-seq))))


;并行拼接
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1)
           (statements seq2))))




(assign val
        (op make-compiled-procedure)
        (label entry2)
        (reg env))
(goto (label after-lambda1))
entry2
(assign env (op compiled-procedure-env) (reg proc))
(assign env
        (op extend-environment)
        (const (n))
        (reg argl)
        (reg env))
⟨compilation of procedure body⟩
after-lambda1
(perform (op define-variable!)
         (const factorial)
         (reg val)
         (reg env))
(assign val (const ok))




(assign proc
        (op lookup-variable-value) (const =) (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val
        (op lookup-variable-value) (const n) (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch17))
compiled-branch16
(assign continue (label after-call15))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch17
(assign val
        (op apply-primitive-procedure)
        (reg proc)
        (reg argl))
after-call15








;完整的factorial编译
;; construct the procedure and skip over code for the procedure body
(assign val
        (op make-compiled-procedure)
        (label entry2)
        (reg env))
(goto (label after-lambda1))
entry2 ; calls to factorial will enter here
(assign env (op compiled-procedure-env) (reg proc))
(assign env
        (op extend-environment)
        (const (n))
        (reg argl)
        (reg env))
;; begin actual procedure body
(save continue)
(save env)
;; compute (= n 1)
(assign proc
        (op lookup-variable-value)
        (const =)
        (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val
        (op lookup-variable-value)
        (const n)
        (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch17))
compiled-branch16
(assign continue (label after-call15))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch17
(assign val
        (op apply-primitive-procedure)
        (reg proc)
        (reg argl))
after-call15 ; val now contains result of (= n 1)
(restore env)
(restore continue)
(test (op false?) (reg val))
(branch (label false-branch4))
true-branch5 ; return 1
(assign val (const 1))
(goto (reg continue))
false-branch4
;; compute and return (* (factorial (- n 1)) n)
(assign proc
        (op lookup-variable-value)
        (const *)
        (reg env))
(save continue)
(save proc) ; save * procedure
(assign val
        (op lookup-variable-value)
        (const n)
        (reg env))
(assign argl (op list) (reg val))
(save argl) ; save partial argument list for *
;; compute (factorial (- n 1)), which is the other argument for *
(assign proc
        (op lookup-variable-value)
        (const factorial)
        (reg env))
(save proc) ; save factorial procedure
;; compute (- n 1), which is the argument for factorial
(assign proc
        (op lookup-variable-value)
        (const -)
        (reg env))
(assign val (const 1))
(assign argl (op list) (reg val))
(assign val
        (op lookup-variable-value)
        (const n)
        (reg env))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch8))
compiled-branch7
(assign continue (label after-call6))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch8
(assign val
        (op apply-primitive-procedure)
        (reg proc)
        (reg argl))
after-call6 ; val now contains result of (- n 1)
(assign argl (op list) (reg val))
(restore proc) ; restore factorial
;; apply factorial
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch11))
compiled-branch10
(assign continue (label after-call9))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch11
(assign val
        (op apply-primitive-procedure)
        (reg proc)
        (reg argl))
after-call9 ; val now contains result of (factorial (- n 1))
(restore argl) ; restore partial argument list for *
(assign argl (op cons) (reg val) (reg argl))
(restore proc) ; restore *
(restore continue)
;; apply * and return its value
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch14))
compiled-branch13
;; note that a compound procedure here is called tail-recursively
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch14
(assign val
        (op apply-primitive-procedure)
        (reg proc)
        (reg argl))
(goto (reg continue))
after-call12
after-if3
after-lambda1
;; assign the procedure to the variable factorial
(perform (op define-variable!)
         (const factorial)
         (reg val)
         (reg env))
(assign val (const ok))



apply-dispatch
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-apply))
(test (op compound-procedure?) (reg proc))
(branch (label compound-apply))
(test (op compiled-procedure?) (reg proc))
(branch (label compiled-apply))
(goto (label unknown-procedure-type))
compiled-apply
(restore continue)
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))



external-entry
(perform (op initialize-stack))
(assign env (op get-global-environment))
(assign continue (label print-result))
(goto (reg val))


(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else (display object))))


(define (compile-and-go expression)
  (let ((instructions
         (assemble
          (statements
           (compile expression 'val 'return))
          eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))



















