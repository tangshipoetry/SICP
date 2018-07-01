#lang racket


(assert! (rule (father ?s ?f) 
               (or (son ?f ?s) 
                   (and (son ?w ?s) 
                        (wife ?f ?w))))) 
  
(assert! (rule (grandson ?g ?s) 
               (and (father ?s ?f) 
                    (father ?f ?g)))) 


;网上的
;;; Query input: 
(grandson Cain ?s) 
  
;;; Query output: 
(grandson Cain Irad) 
;;; Query input: 
(father ?s Lamech) 
  
;;; Query output: 
(father Jubal Lamech) 
(father Jabal Lamech) 
;;; Query input: 
(grandson Methushael ?s) 
  
;;; Query output: 
(grandson Methushael Jubal) 
(grandson Methushael Jabal) 































