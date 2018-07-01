#lang racket

;网上的
;; from the book
(run-query '(assert! (rule (append-to-form () ?y ?y))))
(run-query '(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                           (append-to-form ?v ?y ?z))))
;; base case an empty list
(run-query '(assert! (rule (reverse () ()))))
; reverse a list
; a list, L,  has a head and tail, H . T
; reversing the list is (reverse T) . H
(run-query '(assert! (rule (reverse (?h . ?t) ?y)
                           (and (reverse ?t ?reversed-t)
                                (append-to-form ?reversed-t (?h) ?y)))))





;网上的
rule: 
(assert! (rule (reverse () ()))) 
(assert! (rule (reverse ?x ?y) 
               (and (append-to-form (?first) ?rest ?x) 
                    (append-to-form ?rev-rest (?first) ?y) 
                    (reverse ?rest ?rev-rest)))) 
  
(reverse (1 2 3) ?x)  : infinite loop 
;;; Query input: 
(reverse ?x (1 2 3)) 
  
;;; Query output: 
(reverse (3 2 1) (1 2 3)) 





































