#lang racket


(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))


(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else
           (count-iter (cdr tree)
                       (count-iter (car tree)
                                   n)))))
  (count-iter tree 0))


;网上的
(define count-leaves-machine 
  (make-machine 
   (list (list '+ +) (list 'null? null?) 
         (list 'pair? pair?) (list 'car car) (list 'cdr cdr)) 
   '( 
     (assign continue (label count-leaves-done)) 
     (assign val (const 0)) 
     tree-loop 
     (test (op null?) (reg tree)) ;null?
     (branch (label null-tree)) 
     (test (op pair?) (reg tree)) ;?pair?
     (branch (label left-tree)) 
     (assign val (const 1)) ;counter 1
     (goto (reg continue)) 
     left-tree              ;left-tree
     (save tree) 
     (save continue) 
     (assign continue (label right-tree)) ;assign right-tree
     (assign tree (op car) (reg tree)) 
     (goto (label tree-loop)) 
     right-tree            ;right-tree
     (restore continue) 
     (restore tree) 
     (save continue) 
     (save val) 
     (assign continue (label after-tree)) ;assign after-tree 
     (assign tree (op cdr) (reg tree)) 
     (goto (label tree-loop)) 
     after-tree           ;after-tree
     (assign var (reg val)) 
     (restore val) 
     (restore continue) 
     (assign val (op +) (reg var) (reg val)) 
     (goto (reg continue)) 
     null-tree          ;null-tree
     (assign val (const 0)) 
     (goto (reg continue)) 
     count-leaves-done))) 

;网上的
;;b) 
(define c-m 
  (make-machine '() 
                (list (list 'null? null?) 
                      (list 'pair? pair?) 
                      (list '+ +) 
                      (list 'car car) 
                      (list 'cdr cdr)) 
                '(controller 
                  (assign n (const 0)) 
                  (assign continue (label iter-done)) 
                    
                  iter 
                  (test (op null?) (reg tree)) ;null?
                  (branch (label null-tree)) 
                  (test (op pair?) (reg tree)) ;pair?
                  (branch (label pair-tree)) 
                  (assign n (op +) (reg n) (const 1)) ;counter 1
                  (goto (reg continue)) 
  
                  null-tree 
                  (goto (reg continue)) 
  
                  pair-tree ;pair-tree
                  (save continue) 
                  (save tree) 
                  (assign tree (op car) (reg tree)) 
                  (assign continue (label after-left-tree)) ;assign after-left-tree
                  (goto (label iter)) 
  
                  after-left-tree ;after-left-tree
                  (restore tree) 
                  (assign tree (op cdr) (reg tree)) 
                  (assign continue (label after-right-tree)) ;assign after-right-tree
                  (goto (label iter)) 
  
                  after-right-tree ;after-right-tree
                  (restore continue) 
                  (goto (reg continue)) 
  
                  iter-done))) 





;网上的——没看
;; a. 
(define (not-pair? lst)
  (not (pair? lst)))
  
(define count-leaves 
  (make-machine 
   `((car ,car) (cdr ,cdr) (null? ,null?) 
                (not-pair? ,not-pair?) (+ ,+)) 
   '( 
     start 
     (assign continue (label done)) 
     (assign n (const 0)) 
     count-loop 
     (test (op null?) (reg lst)) 
     (branch (label null)) 
     (test (op not-pair?) (reg lst)) 
     (branch (label not-pair)) 
     (save continue) 
     (assign continue (label after-car)) 
     (save lst) 
     (assign lst (op car) (reg lst)) 
     (goto (label count-loop)) 
     after-car 
     (restore lst) 
     (assign lst (op cdr) (reg lst)) 
     (assign continue (label after-cdr)) 
     (save val) 
     (goto (label count-loop)) 
     after-cdr 
     (restore n) 
     (restore continue) 
     (assign val 
             (op +) (reg val) (reg n)) 
     (goto (reg continue)) 
     null 
     (assign val (const 0)) 
     (goto (reg continue)) 
     not-pair 
     (assign val (const 1)) 
     (goto (reg continue)) 
     done))) 
  
;; b. 
  
(define count-leaves 
  (make-machine 
   `((car ,car) (cdr ,cdr) (pair? ,pair?) 
                (null? ,null?) (+ ,+)) 
   '( 
     start 
     (assign val (const 0)) 
     (assign continue (label done)) 
     (save continue) 
     (assign continue (label cdr-loop)) 
     count-loop 
     (test (op pair?) (reg lst)) 
     (branch (label pair)) 
     (test (op null?) (reg lst)) 
     (branch (label null)) 
     (assign val (op +) (reg val) (const 1)) 
     (restore continue) 
     (goto (reg continue)) 
     cdr-loop 
     (restore lst) 
     (assign lst (op cdr) (reg lst)) 
     (goto (label count-loop)) 
     pair 
     (save lst) 
     (save continue) 
     (assign lst (op car) (reg lst)) 
     (goto (label count-loop)) 
     null 
     (restore continue) 
     (goto (reg continue)) 
     done))) 





































