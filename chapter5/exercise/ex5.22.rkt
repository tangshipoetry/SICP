#lang racket



;网上的
; a. append  
(define append-machine 
  (make-machine 
   `((null? ,null?) (cons ,cons) (car ,car) 
                    (cdr ,cdr)) 
   '( 
     start 
     (assign x (reg x))               ; these 2 instruction are only here to 
     (assign y (reg y))               ; initialize the registers.  
     (assign continue (label done))   ; retrun addres 
     (save continue)                  ; save it. 
     append 
     (test (op null?) (reg x)) 
     (branch (label null)) 
     (assign temp (op car) (reg x))   ; push car as the arg to cons. 
     (save temp) 
     (assign continue (label after-rec)) ;return address for procedure call. 
     (save continue)                  ; push the return address 
     (assign x (op cdr) (reg x))      ; arg for recursive call to append. 
     (goto (label append))            ; recursive call to append. 
     after-rec 
     (restore x)                      ; get the argument pushed by append  
     (assign val (op cons) (reg x) (reg val)) ; consit to the return value 
     (restore continue)               ; get the return address 
     (goto (reg continue))            ; return to caller.  
     null 
     (assign val (reg y))             ; base case, return value = y. 
     (restore continue)               ; get return address 
     (goto (reg continue))            ; return to caller. 
     done))) 



;------------------------------------------------------------没看明白----------------------------------------------------------------------------
; b. append! 
(define append!-machine 
  (make-machine 
   `((set-cdr! ,set-cdr!) (null? ,null?) 
                          (cdr ,cdr)) 
   '( 
     start 
     (assign x (reg x))               ; as before just initiailze the regs. 
     (assign y (reg y)) 
     (assign temp1 (reg x))           ; must use temp to avoid changing x.  
     (goto (label last-pair)) 
     append! 
     (assign temp (op set-cdr!) (reg temp1) (reg y)) ;set-cdr! returns an 
     (goto (label done))              ; unspecified value, that we put in temp. 
     last-pair                          ; we want the side effect. 
     (assign temp (op cdr) (reg temp1)) ; test if (cdr temp1 is null) 
     (test (op null?) (reg temp))     ; if so, temp1 is the last pair. 
     (branch (label null)) 
     (assign temp1 (op cdr) (reg temp1)) 
     null 
     (goto (label append!))           ; splice the lists. 
     done 
     ))) 






























































