#lang racket


;a. Recursive exponentiation
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(controller
 (assign continue (label fact-done))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label base-case))
 (save continue)
 (save n)
 (assign n (op -) (reg n) (const 1))
 (assign continue (label after-expt))
 (goto (label expt-loop))
 after-expt
 (restore n)
 (restore continue)
 (assign val (op *) (reg b) (reg val)) ;val now contains n(n - 1)!
 (goto (reg continue)) ;return to caller
 base-case
 (assign val (const 1)) ;base case: 1! = 1
 (goto (reg continue)) ;return to caller
 fact-done)


;b. Iterative exponentiation:
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))


(controller
 (assign continue (label fact-done))
 (assign counter (const n))
 (assign product (const 1))
 expt-iter
 (test (op =) (reg n) (const 0))
 (assign counter (op -) (reg counter) (const 1))
 (assign product (reg product) (reg b))
 (goto expt-iter)
 fact-done)






























