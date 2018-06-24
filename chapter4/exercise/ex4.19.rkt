#lang racket

;实际情况如Alyssa所说，报错
(define (em)
  (let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10)))


;通用有效的机制这里实现不出来,scheme采用报错的方式强制要求编写更好的程序


;网上的，还没仔细看
I will describe how to implement Eva's scheme (no pun intended).

One way to do so is to topologically sort the non-function definitions in order of dependency. This can be done by converting the sequence of definitions into a directed graph according to the interdependency of the variables. Meanwhile, we can check the graph for cycles and signal an error if found, and evaluate the definitions in topologically sorted order if no cycles are found.

Needless to say, trying to implement directed acyclic graphs in Scheme, to say nothing of topological sort, is probably non-trivial and may arguably be overkill for implementing a measly little Scheme interpreter.

Hence, here is a conceptually easier way to do it. We first take out all the function definitions and put them at the top, since their bodies are delayed and hence will not pose any issues whatsoever. Then, we take the list of the non-function definitions, generate a list of the matching dependent variables in each body, and repeatedly take out all non-function definitions whose bodies are independent of any remaining non-function variables. This is probably asymptotically slower than topological sort, but it works fine, and has the added bonus of being able to naturally check for cycles.

First, we redefine make-procedure: (note I am assuming all code in chapter 4 up to this exercise has been evaluated, so all the helper functions used by the book to redefine eval is available).

 (define (function-definition? exp) 
   (and (definition? exp) 
        (lambda? (definition-value exp)))) 
  
 (define (non-function-definition? exp) 
   (and (definition? exp) 
        (not (function-definition? exp)))) 
  
 (define (reorder-procedure-body body) 
   (let ((func-defs (filter function-definition? body)) 
         (var-defs (filter non-function-definition? body)) 
         (non-defs (remove definition? body))) 
     (append func-defs 
             (reorder-non-function-definitions var-defs) 
             non-defs))) 
  
 (define (make-procedure parameters body env) 
   (list 'procedure 
         parameters 
         (reorder-procedure-body body) 
         env)) 
Then, a few helper functions:

 ;; unrolls nested lists 
 (define (tree->list tree) 
   (if (list? tree) 
       (apply-in-underlying-scheme 
        append 
        (map tree->list tree)) 
       (list tree))) 
  
 ;; removes duplicates 
 (define (list->set lst) 
   (if (or (null? lst) 
           (null? (cdr lst))) 
       lst 
       (cons (car lst) 
             (delete (car lst) 
                     (list->set (cdr lst)))))) 
  
 (define (all-included-symbols symbol-pool seq) 
   (intersection-set symbol-pool 
                     (list->set (tree->list seq)))) 
 ;; intersection-set is given in chapter 2 of SICP 
  
 ;; there are likely faster ways to do this 
 ;; computes set1 - set2 nondestructively 
 (define (difference-set set1 set2) 
   (define (in-set2? obj1) 
     (find (lambda (obj2) (eq? obj1 obj2)) 
           set2)) 
   (remove in-set2? set1)) 
Finally, the main workhorse function:

 ;; assume no duplicate variables in var-defs, otherwise undefined behavior 
 (define (reorder-non-function-definitions var-defs) 
   (define (no-dependencies? pair) 
     (null? (cdr pair))) 
   ;; pair here means definition / included symbol pair 
   (define (pairs-with-symbols-removed pairs symbols) 
     (map (lambda (pair) 
            (cons (car pair) (difference-set (cdr pair) symbols))) 
          pairs)) 
   (define (iter pairs-defs-included result) 
     (if (null? pairs-defs-included) 
         result 
         (let ((independent (filter no-dependencies? pairs-defs-included)) 
               (dependent (remove no-dependencies? pairs-defs-included))) 
           (if (null? independent) 
               (error "cycle detected in inner non-function defines") 
               (let ((symbols-to-remove 
                      (map (lambda (pair) 
                             (definition-variable (car pair))) 
                           independent))) 
                 (iter 
                  (pairs-with-symbols-removed dependent symbols-to-remove) 
                  (append (map car independent) result))))))) 
   (let* ((symbol-pool (map definition-variable var-defs)) 
          (pairs-defs-included 
           (map (lambda (def) 
                  (cons def (all-included-symbols symbol-pool 
                                                  (definition-value def)))) 
                var-defs))) 
     (reverse (iter pairs-defs-included '())))) 
 ;; need to reverse because results built using cons, in reverse order 
Now, assuming that eval is *not* the builtin eval but rather the simplified one that has been defined as in the code from the SICP text, here are some examples:

 (assert (equal? '((define (f x) 7) (define a 3) (define b a)) 
                 (reorder-procedure-body 
                  '((define a 3) (define b a) (define (f x) 7))))) 
  
 ;; example from the exercise 
 (assert (= 20 (eval '(let ((a 1)) 
                        (define (f x) 
                          (define b (+ a x)) 
                          (define a 5) 
                          (+ a b)) 
                        (f 10)) the-global-environment))) 
 ;; 20, as Eva required. 
I ran all code above in MIT Scheme.

note we are assuming the bodies of the non-function definitions do not contain redefinitions of variables shared with other non-function definitions. For example, the following should be perfectly legal but may break our program and result in undefined behavior:

 (define (f) 
   (define a 5) 
   (define b  
     (let ((a 6))  
       a)) 
   b) 
This is an admitted limitation of our program: to account for this case requires significant further work.

























