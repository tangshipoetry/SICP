#lang racket

;because all the answers satisfy the rule. 
;we can sort the person in alphabetic order, then get only one pair. 
(define (person->string person) 
  (if (null? person) 
      "" 
      (string-append (symbol->string (car person)) (person->string (cdr person))))) 
(define (person>? p1 p2) 
  (string>? (person->sring p1) (person->string p2))) 
  
(assert! (rule (asy-lives-near ?person1 ?person2) 
               (and (address ?person1 (?town . ?rest-1)) 
                    (address ?person2 (?town . ?rest-2)) 
                    (lisp-value person>? ?person1 ?person2)))) 











































