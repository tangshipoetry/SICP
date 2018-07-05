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























