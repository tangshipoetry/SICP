#lang racket


;赋值
(assign ⟨reg1⟩ (op car) (reg ⟨reg2⟩))
(assign ⟨reg1⟩ (op cdr) (reg ⟨reg2⟩))

(assign ⟨reg1⟩ (op vector-ref) (reg the-cars) (reg ⟨reg2⟩))
(assign ⟨reg1⟩ (op vector-ref) (reg the-cdrs) (reg ⟨reg2⟩))


;修改
(perform (op set-car!) (reg ⟨reg1⟩) (reg ⟨reg2⟩))
(perform (op set-cdr!) (reg ⟨reg1⟩) (reg ⟨reg2⟩))

(perform
 (op vector-set!) (reg the-cars) (reg ⟨reg1⟩) (reg ⟨reg2⟩))
(perform
 (op vector-set!) (reg the-cdrs) (reg ⟨reg1⟩) (reg ⟨reg2⟩))

;cons
(assign ⟨reg1⟩ (op cons) (reg ⟨reg2⟩) (reg ⟨reg3⟩))

(perform
 (op vector-set!) (reg the-cars) (reg free) (reg ⟨reg2⟩))
(perform
 (op vector-set!) (reg the-cdrs) (reg free) (reg ⟨reg3⟩))
(assign ⟨reg1⟩ (reg free))
(assign free (op +) (reg free) (const 1))

;eq?
(op eq?) (reg ⟨reg1⟩) (reg ⟨reg2⟩)


;堆栈实现
;save
(assign the-stack (op cons) (reg ⟨reg⟩) (reg the-stack))
;restore
(assign ⟨reg⟩ (op car) (reg the-stack))
(assign the-stack (op cdr) (reg the-stack))
;初始化
(assign the-stack (const ()))


























