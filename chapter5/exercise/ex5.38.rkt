#lang racket

;网上的
;(a) 
;; in compile
((open-code? exp) (compile-open-code exp target linkage))
  
(define (open-code? exp)
  (memq (car exp) '(+ - * /)))
  
(define (spread-arguments operand1 operand2) 
  (let ((op1 (compile operand1 'arg1 'next)) 
        (op2 (compile operand2 'arg2 'next))) 
    (list op1 op2))) 
  
;(b) 
  
;; This procedure has a bug. It does not save the environment 
;; Around the compilation of the first arg. Becuase of this it 
;; will give incorrect results for recursive procedures. In my answer 
;; Below I have fixed this. 
(define (compile-open-code exp target linkage) 
  (let ((op (car exp)) 
        (args (spread-arguments (cadr exp) (caddr exp)))) 
    (end-with-linkage linkage 
                      (append-instruction-sequences 
                       (car args) 
                       (preserving '(arg1) 
                                   (cadr args) 
                                   (make-instruction-sequence '(arg1 arg2) (list target) 
                                                              `((assign ,target (op ,op) (reg arg1) (reg arg2))))))))) 



;网上的
(define (+? exp)
  (tagged-list? exp '+))
;;设定只处理两个参数的情况 
(define (spread-arguments argl) 
  (let ((operand-code1 (compile (car argl) 'arg1 'next)) 
        (operand-code2 (compile (cadr argl) 'arg2 'next))) 
    (preserving '(env) 
                operand-code1 
                (make-instruction-sequence 
                 (list-union '(arg1) 
                             (registers-needed operand-code2)) 
                 (list-difference (registers-modified operand-code2) 
                                  '(arg1)) 
                 (append '((save arg1)) 
                         (statements operand-code2) 
                         '((restore arg1))))))) 
(define (compile-+ exp target linkage) 
  (let ((operand-codes (spread-arguments (operands exp)))) 
    (end-with-linkage 
     linkage 
     (preserving '(continue) 
                 operand-codes 
                 (make-instruction-sequence 
                  '() 
                  `(target) 
                  `((assign ,target (op +) (reg arg1) (reg arg2)))))))) 
  
(define t7 (compile-test '(+ (+ a 1) (+ 3 2)))) 
;;the result of t7 
(assign arg1 (op lookup-variable-value) (const a) (reg env)) 
(save arg1) 
(assign arg2 (const 1)) 
(restore arg1) 
(assign arg1 (op +) (reg arg1) (reg arg2)) 
(save arg1) 
(assign arg1 (const 3)) 
(save arg1) 
(assign arg2 (const 2)) 
(restore arg1) 
(assign arg2 (op +) (reg arg1) (reg arg2)) 
(restore arg1) 
(assign val (op +) (reg arg1) (reg arg2)) 
  
;;d 
(define (compile-++ exp target linkage) 
  (compile-+ (construct exp) target linkage)) 
(define (construct exp) 
  (if (> (length (operands exp)) 
         2) 
      (append (list (car exp) 
                    (cadr exp)) 
              (list (append (list (car exp)) 
                            (cddr exp)))) 
      exp)) 














































