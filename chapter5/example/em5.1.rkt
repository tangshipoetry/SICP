#lang racket



;GCD规范描述
(data-paths
 (registers
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))
 (operations
  ((name rem) (inputs (register a) (register b)))
  ((name =) (inputs (register b) (constant 0)))))
(controller
 test-b ; label
 (test =) ; test
 (branch (label gcd-done)) ; conditional branch
 (t<-r) ; buon push
 (a<-b) ; buon push
 (b<-t) ; buon push
 (goto (label test-b)) ; unconditional branch
 gcd-done) ; label






;数据路图和控制器合一
(controller
 test-b
 (test (op =) (reg b) (const 0))
 (branch (label gcd-done))
 (assign t (op rem) (reg a) (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label test-b))
 gcd-done)




;读取输入并打印结果的GCD机器
(controller
 gcd-loop
 (assign a (op read))
 (assign b (op read))
 test-b
 (test (op =)
       (reg b)
       (const 0))
 (branch (label gcd-done))
 (assign t
         (op rem)
         (reg a)
         (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label test-b))
 gcd-done
 (perform (op print)
          (reg a))
 (goto (label gcd-loop)))



;GCD操作中将取余操作拆分为循环
(controller test-b
            (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (reg a))
            rem-loop
            (test (op <) (reg t) (reg b))
            (branch (label rem-done))
            (assign t (op -) (reg t) (reg b))
            (goto (label rem-loop))
            rem-done
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
            gcd-done)




;figure5.8
gcd-1
(test (op =) (reg b) (const 0))
(branch (label after-gcd-1))
(assign t (op rem) (reg a) (reg b))
(assign a (reg b))
(assign b (reg t))
(goto (label gcd-1))
after-gcd-1
: : :
gcd-2
(test (op =) (reg b) (const 0))
(branch (label after-gcd-2))
(assign t (op rem) (reg a) (reg b))
(assign a (reg b))
(assign b (reg t))
(goto (label gcd-2))
after-gcd-2

;Figure 5.9
gcd
(test (op =) (reg b) (const 0))
(branch (label gcd-done))
(assign t (op rem) (reg a) (reg b))
(assign a (reg b))
(assign b (reg t))
(goto (label gcd))
gcd-done
(test (op =) (reg continue) (const 0))
(branch (label after-gcd-1))
(goto (label after-gcd-2))
: : :
;; Before branching to gcd from the first place where it is needed, we place 0 in the continue register
(assign continue (const 0))
(goto (label gcd))
after-gcd-1
: : :
;; Before the second use of gcd, we place 1 in the continue register
(assign continue (const 1))
(goto (label gcd))
after-gcd-2



;Figure 5.10: # Assigning labels to the continue register simplifies and generalizes the strategy shown in Figure 5.9.
gcd
(test (op =) (reg b) (const 0))
(branch (label gcd-done))
(assign t (op rem) (reg a) (reg b))
(assign a (reg b))
(assign b (reg t))
(goto (label gcd))
gcd-done
(goto (reg continue))
: : :
;; Before calling gcd, we assign to continue
;; the label to which gcd should return.
(assign continue (label after-gcd-1))
(goto (label gcd))
after-gcd-1
: : :
;; Here is the second call to gcd,
;; with a different continuation.
(assign continue (label after-gcd-2))
(goto (label gcd))
after-gcd-2








