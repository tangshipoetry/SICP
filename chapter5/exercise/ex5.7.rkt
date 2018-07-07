#lang racket

;a. Recursive exponentiation
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))


(define expt-machine
  (make-machine
   (list b n continue val)
   (list (list '- -) (list '= =) (list '* *))
   '(test-n
     (test (op =) (reg n) (const 0))
     (goto base)
     (save continue)
     (assign continue aft-expt)
     (assign n (op -) (reg n) (const 1))
     (goto test-n)
     aft-expt
     (assign val (op *)(reg val)(reg b))
     (restore continue)
     (goto (reg continue))
     base
     (assign val (const 1))
     (goto (reg continue))
     expt-done)))



;b. Iterative exponentiation:
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1)
                   (* b product))))
  (expt-iter n 1))


(define expt-machine
  (make-machine
   (list b n product counter)
   (list (list '= =)(list '- -)(list '* *))
   (test-counter
    (test (op =) (reg counter) (const 0))
    (goto expt-done)
    (assign product (op *) (reg product) (reg b))
    (assign counter (op -)(reg counter) (const 1))
    (goto test-counter)
    expt-done)))



















































