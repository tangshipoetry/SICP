#lang racket

;网上的
;; I skip compile-assignment. 
(define (compile-variable exp target linkage ct-env)
  (let ((r (find-variable exp ct-env)))
    (if (eq? r 'not-found)
        (end-with-linkage linkage
                          (make-instruction-sequence '(env) (list target) 
                                                     `((assign ,target 
                                                               (op lookup-variable-value) 
                                                               (const ,exp) 
                                                               (reg env))))) 
        (end-with-linkage linkage 
                          (make-instruction-sequence '(env) (list target) 
                                                     `(assign ,target 
                                                              (op lexical-address-lookup) 
                                                              (const ,r) 
                                                              (reg env))))))) 

;网上的
(define (compile-variable exp target linkage compile-time-environment) 
  (let ((lexical-addr (find-variable exp compile-time-environment))) 
    (end-with-linkage 
     linkage 
     (make-instruction-sequence 
      '(env) (list target) 
      (if (eq? 'not-found lexical-addr) 
          `((assign ,target 
                    (op lookup-variable-value) 
                    (const ,exp) 
                    (reg env))) 
          `((assign ,target 
                    (op lexical-address-lookup) 
                    (const ,lexical-addr) 
                    (reg env)))))))) 
  
(define (compile-assignment exp target linkage compile-time-environment) 
  (let ((var (assignment-variable exp)) 
        (get-value-code 
         (compile (assignment-value exp) 'val 'next compile-time-environment))) 
    (let ((lexical-addr (find-variable var compile-time-environment))) 
      (end-with-linkage 
       linkage 
       (preserving 
        '(env) 
        get-value-code 
        (make-instruction-sequence 
         '(env val) (list target) 
         (if (eq? lexical-addr 'not-found) 
             `((perform (op set-variable-value!) 
                        (const ,var) 
                        (reg val) 
                        (reg env)) 
               (assign ,target (const ok))) 
             `((perform (op lexical-address-set!) 
                        (const ,lexical-addr) 
                        (reg val) 
                        (reg env)))))))))) 
  
(compile '((lambda (x y) 
             (+ x y)) 
           1 2) 
         'val 
         'next 
         the-empty-environment) 
  
; after compiling, and simulating -> 3 
  
  
(compile '(((lambda (x y) 
              (lambda (a b c d e) 
                ((lambda (y z) (* x y z)) 
                 (* a b x) 
                 (+ c d x)))) 
            3 4) 
           1 2 3 4 5) 
         'val 
         'next 
         the-empty-environment) 
; generates and 89 instruction sequence machine, and computes 180. 
; as expected. 






























