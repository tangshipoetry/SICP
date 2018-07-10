#lang racket



;网上的
;; if the variable is unbounded, cons 'unbound with it;  
;; otherwise, cons 'bounded with it. then we can use 
;; car to judge it's bounded or not. 
(define (lookup-variable-value var env) 
  (define (env-loop env) 
    (define (scan vars vals) 
      (cond ((null? vars) 
             (env-loop (enclosing-environment env))) 
            ((eq? var (car vars)) 
             (cons 'bounded (car vals)))              ;; **** 
            (else (scan (cdr vars) (cdr vals))))) 
    (if (eq? env the-empty-environment) 
        (cons 'unbounded '())                        ;; *** 
        (let ((frame (first-frame env))) 
          (scan (frame-variables frame) 
                (frame-values frame))))) 
  (env-loop env)) 
(define (bound-variable? var) 
  (and (pair? var) (eq? (car var) 'bounded))) 
(define (extract-variable-value var) 
  (cdr var)) 
  
ev-variable 
(assign val (op lookup-variable-value) (reg exp) (reg env)) 
(test (op bound-variable?) (reg val)) 
(branch (label bound-variable)) 
(assign val (const unbounded-variable-error)) 
(goto (label signal-error))  
bound-variable 
(assign val (op extract-variable-value) (reg val)) 
(goto (reg continue)) 



;网上的

(define safe-primitives 
  (list car cdr /)) 
(define (apply-primitive-procedure proc args) 
  (if debug 
      (display (list 'apply-primitive proc args)) (newline)) 
  (let ((primitive (primitive-implementation proc))) 
    (if (member primitive safe-primitives) 
        (safe-apply primitive args)     ; ex 5.30 b 
        (apply-in-underlying-scheme 
         primitive args)))) 
  
(define (safe-apply proc args)          ; ex 5.30 b 
  (if debug 
      (display 'safe-apply) (newline)) 
  (cond ((or (eq? proc car) 
             (eq? proc cdr)) 
         (safe-car-cdr proc args)) 
        ((eq? proc /) 
         (safe-division proc args)) 
        (else 
         (list 'primitive-error proc args)))) 
  
(define (primitive-error? val)          ; ex 5.30 b 
  (tagged-list? val 'primitive-error)) 
  
(define (safe-car-cdr proc args)        ; ex 5.30 b 
  (if debug 
      (display (list 'safe-car-cdr args)) 
      (newline)) 
  (if (not (pair? (car args)))          ; args is a list (args '()) 
      (list 'primitive-error 'arg-not-pair) 
      (apply-in-underlying-scheme proc args))) 
(define (safe-division proc args)       ; ex 5.30 b 
  (if (= 0 (cadr args)) 
      (cons 'primitive-error 'division-by-zero) 
      (apply-in-underlying-scheme proc args))) 
  
; and this is added to the evaluator. 
  
primitive-apply 
(assign val (op apply-primitive-procedure) 
        (reg proc) 
        (reg argl)) 
(test (op primitive-error?) (reg val)) ; ex 5.30 b 
(branch (label primitive-error)) 
(restore continue) 
(goto (reg continue)) 
  
primitive-error 
(restore continue)               ; clean up stack from apply-dispatch 
(goto (label signal-error))






























