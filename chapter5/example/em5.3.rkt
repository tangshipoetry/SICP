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


begin-garbage-collection
(assign free (const 0))
(assign scan (const 0))
(assign old (reg root))
(assign relocate-continue (label reassign-root))
(goto (label relocate-old-result-in-new))
reassign-root
(assign root (reg new))
(goto (label gc-loop))

gc-loop
(test (op =) (reg scan) (reg free))
(branch (label gc-flip))
(assign old (op vector-ref) (reg new-cars) (reg scan))
(assign relocate-continue (label update-car))
(goto (label relocate-old-result-in-new))

update-car
(perform (op vector-set!)
         (reg new-cars)
         (reg scan)
         (reg new))
(assign old (op vector-ref) (reg new-cdrs) (reg scan))
(assign relocate-continue (label update-cdr))
(goto (label relocate-old-result-in-new))
update-cdr
(perform (op vector-set!)
         (reg new-cdrs)
         (reg scan)
         (reg new))
(assign scan (op +) (reg scan) (const 1))
(goto (label gc-loop))

relocate-old-result-in-new              ;将旧值移动到新的位置
(test (op pointer-to-pair?) (reg old))
(branch (label pair))
(assign new (reg old))                  ;非序对,直接赋值
(goto (reg relocate-continue))
pair                                    ;序对操作
(assign oldcr (op vector-ref) (reg the-cars) (reg old));oldcr设置为旧值的car
(test (op broken-heart?) (reg oldcr))   ;判断内容是否已经移动
(branch (label already-moved))
(assign new (reg free))                 ;为序对分配新位置空间
(assign free (op +) (reg free) (const 1));更新指针
;; Copy the car and cdr to new memory.
(perform (op vector-set!)
         (reg new-cars) (reg new) (reg oldcr))
(assign oldcr (op vector-ref) (reg the-cdrs) (reg old));oldcr设置为旧值的cdr
(perform (op vector-set!)
         (reg new-cdrs) (reg new) (reg oldcr))
;; Construct the broken heart.
(perform (op vector-set!)
         (reg the-cars) (reg old) (const broken-heart))
(perform
 (op vector-set!) (reg the-cdrs) (reg old) (reg new))
(goto (reg relocate-continue))
already-moved
(assign new (op vector-ref) (reg the-cdrs) (reg old))
(goto (reg relocate-continue))

gc-flip
(assign temp (reg the-cdrs))
(assign the-cdrs (reg new-cdrs))
(assign new-cdrs (reg temp))
(assign temp (reg the-cars))
(assign the-cars (reg new-cars))
(assign new-cars (reg temp))











































