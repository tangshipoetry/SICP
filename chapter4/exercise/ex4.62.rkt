#lang racket

;网上的
rule: 
(assert! (rule (last-pair (?x) (?x)))) 
(assert! (rule (last-pair (?u . ?v) (?x)) 
               (last-pair ?v (?x)))) 
  
;;; Query input: 
(last-pair (3) ?x)
  
;;; Query output: 
(last-pair (3) (3))
;;; Query input: 
(last-pair (1 2 3) ?x)
  
;;; Query output: 
(last-pair (1 2 3) (3))
;;; Query input: 
(last-pair (2 ?x) (3))
  
;;; Query output: 
(last-pair (2 3) (3))
  
there is no answer for (last-pair ?x (3))










































