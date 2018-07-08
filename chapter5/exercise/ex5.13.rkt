#lang racket

;网上的
;In make-new-machine, change the code of lookup-register 
(define (lookup-register name) 
  (let ((val (assoc name register-table))) 
    (if val 
        (cadr val) 
        (begin 
          (allocate-register name) 
          (lookup-register name)))))







































