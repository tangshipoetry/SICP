#lang racket
;自己写的
(controller
 test-n
 (test (op >) (reg n) (const N))
 (branch (label factorial-done))
 (assign p (op *) (reg p) (reg n))
 (assign n (op *) (reg n) (reg 1))
 (goto (label test-b))
 factorial-done)


;网上的
(controller  
 (assign p (const 1)) 
 (assign c (const 1)) 
  
 test-n 
 (test (op >) (reg c) (reg n)) 
 (branch (label done)) 
 (assign x (op *) (reg p) (reg c)) 
 (assign y (op +) (reg c) (const 1)) 
 (assign p (reg x)) 
 (assign c (reg y)) 
 (goto (label test-n)) 
  
 done) 































