#lang racket

;网上的
;1
(define (list-amb li)
   (if (null? li)
       (amb)
       (amb (car li) (list-amb (cdr li)))))
  
 (define (parse-word word-list)
   (require (not (null? *unparsed*)))
   (require (memq (car *unparsed*) (cdr word-list)))
   (let ((found-word (car *unparsed*)))
     (set! *unparsed* (cdr *unparsed*))
     (list-amb (cdr word-list))))   ;; change 

;2

(define (parse-word2 word-list) 
   (require (not (null? *unparsed*))) 
   (set! *unparsed* (cdr *unparsed*)) 
   (list (car word-list) (amb (cdr word-list))))





































