#lang racket

;自己写的——和全局变量搞混了
(define (lexical-address-lookup lexi-add env)
  (let((frame-number (car lexi-add)))
    (if(= frame-number 0)
       (let ((displacement-number (cdr lexi-add))
             (frame (car env)))
         (let((var-val (ref-number frame displacement-number)))
           (if(eq? '*unassigned* (cdr var-val))
              (error "var is unassigned" (car var-val))
              (cdr var-val))))
       (begin
         (set-car! lexi-add (- frame-number 1))
         (lexical-address-lookup lexi-add (cdr env))))))


;网上的
(define (lexical-address addr-frame addr-offset) 
  (cons addr-frame addr-offset)) 
(define (addr-frame address) (car address)) 
(define (addr-offset address) (cdr address)) 
  
(define (lexical-address-lookup env address) 
  (let* ((frame (list-ref env (addr-frame address))) 
         (value (list-ref (frame-values frame) (addr-offset address)))) 
    (if (eq? value '*unassigned*) 
        (error "the variable is unassigned -- LEXICAL-ADDRESS-LOOKUP" address))) 
  value) 
  
  
(define (lexical-address-set! env address value) 
  (let ((frame (addr-frame address)) 
        (offset (addr-frame address))) 
    (define (set-value! f pos) 
      (if (= f 0) 
          (set-car! f value) 
          (set-value! (cdr f (- pos 1))))) 
    (set-value! frame offset value))) 


;网上的
;;my frame looks like this 
(define (make-frame variables values) 
  (map list variables values)) 
  
(define (lexical-address-lookup address env) 
  (let ((value (cadr (list-ref (list-ref env 
                                         (car address)) 
                               (cadr address))))) 
    (if (eq? value 
             '*unassigned*) 
        (error "Unassigned variable! -- LEXICAL-ADDRESS-LOOKUP" address) 
        value))) 
(define (lexical-address-set! address env value) 
  (let ((binding (list-ref (list-ref env 
                                     (car address)) 
                           (cadr address)))) 
    (set-cdr! binding value))) 



































