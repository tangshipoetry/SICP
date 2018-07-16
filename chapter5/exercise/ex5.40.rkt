#lang racket

;网上的，看不懂
(define (compile-lambda-body exp proc-entry ct-env) 
  (let ((formals (lambda-parameters exp))) 
    (append-instruction-sequences 
     (make-instruction-sequence '(env proc argl) '(env) 
                                `(,proc-entry 
                                  (assign env (op compiled-procedure-env) (reg proc)) 
                                  (assign env 
                                          (op extend-environment) 
                                          (const ,formals) 
                                          (reg argl) 
                                          (reg env)))) 
     (compile-sequence 
      (lambda-body exp) 
      'val 'return 
      (extend-ct-env ct-env formals))))) 
(define (extend-ct-env env frame) 
  (cons frame env)) 














































