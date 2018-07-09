#lang racket

;网上的
;;my solution trace the "goto" inst and "branch" inst to update the current-label. need a little more work to store the first-label 
  
;;add this at the begin of "make-new-machine" proc,  
(current-label 'first-label) 
  
;;update execute 
(define (execute) 
  (let ((insts (get-contents pc))) 
    (if (null? insts) 
        'done 
        (let ((inst (car insts))) 
          (begin (cond ((trace-on) 
                        (display current-label);*** 
                        (newline) 
                        (display (instruction-text inst)) 
                        (newline))) 
                 ((instruction-execution-proc inst)) 
                 (set! instruction-number (+ instruction-number 1)) 
                 ;*** 
                 (if (or (tagged-list? (instruction-text inst) 'goto) 
                         (and (tagged-list? (instruction-text inst) 'branch) 
                              (get-contents flag))) 
                     (set! current-label 
                           (label-exp-label (cadr (instruction-text))))) 
                 ;*** 
                 (execute)))))) 








































