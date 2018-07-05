#lang racket


(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))





;自己的
(controller
 sqrt-loop
 (assign result (op read))
 test-result
 (test (op good-enough) (reg result) (reg x))
 (branch (label sqrt-done))
 (assign t (op improve) (reg result))
 (goto (label test-result))
 sqrt-done
 (perform (op print) (reg result))
 (goto sqrt-loop))

(improve
 (assign t (op avg)((op +) (reg result) (reg x))))


;网上的
(define sqrt-machine 
  (make-machine 
   '(x guess temp) 
   (list (list '- -) (list '< <) (list '/ /) (list '+ +) (list '* *) (list '> >)) 
   '((assign guess (const 1.0)) 
     test-g 
     (assign temp (op *) (reg guess) (reg guess)) 
     (assign temp (op -) (reg temp) (reg x)) 
     (test (op >) (reg temp) (const 0)) 
     (branch (label iter)) 
     (assign temp (op -) (const 0) (reg temp)) 
     iter 
     (test (op <) (reg temp) (const 0.001)) 
     (branch (label sqrt-done)) 
     (assign temp (op /) (reg x) (reg guess)) 
     (assign temp (op +) (reg temp) (reg guess)) 
     (assign guess (op /) (reg temp) (const 2)) 
     (goto (label test-g)) 
     sqrt-done)))





















