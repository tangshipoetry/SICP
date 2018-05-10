#lang racket

;摘抄于网上

 ;a. Implement get-record, based on a foo x division table. 
 ;   Each division file must have a division func to extrat  
 ;   'type' and provide a instalation package to include  
 ;   specific get-record. The output is a tagged record. 
 ;   Get-record must return false if doesn't find employee record (c). 
     (define (attach-tag type-tag content) (cons type-tag content)) 
     (define (get-record employee-id file) 
         (attach-tag (division file)  
                     ((get 'get-record (division file)) employee-id file))) 
  
 ;b. get-salary 
     (define (get-salary record) 
         (let ((record-type (car record)) 
               (record-content (cdr record))) 
                 ((get 'get-salary record-type) record-content))) 
  
 ;c. find-employee-record 
     (define (find-employee-record employee-id file-list) 
         (if (null? file-list) 
             #f 
             (let ((current-file (car file-list))) 
              (if (get-record employee-id current-file) 
                 (get-record employee-id current-file) 
                 (find-employee-record employee-id (cdr file-list)))))) 
               
 ;d. New company must provide a installation package for its 
 ;   record-file as new division. This instalation must include 
 ;   the new division get-record and get-salary implementations. 






;-----------------------------------------------------------------------------------------------------------------------------




;; a) 
 (define (get-record division employee-name) 
   ((get division 'record) employee-name)) 
  
 ;; b) 
 (define (get-salary division record) 
   ((get division 'salary) record)) 
  
 ;; c) 
 (define (find-employee-record employee-name division-list) 
   (if (null? division-list) 
       #f 
       (or (get-record (car division-list) employee-name) 
           (find-employee-record employee-name (cdr division-list)))))

































