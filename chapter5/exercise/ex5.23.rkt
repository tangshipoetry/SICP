#lang racket


;网上的
;;  add those following ev-dispatch 
(test (op cond?) (reg expr)) 
(branch (label ev-cond)) 
  
ev-cond 
(assign expr (op cond->if) (reg expr)) 
(goto (label ev-if)) 
  
;; add those to eceval-operations 
(list 'cond? cond?) 
(list 'cond->if cond->if) 







































