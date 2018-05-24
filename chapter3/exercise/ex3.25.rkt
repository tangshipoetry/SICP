#lang sicp

#|
自己写的始终有问题
(define (make-table)
  (define (assoc key records)
    (cond((null? records) #f)
         ((equal? key (caar records)) (car records))
         (else (assoc key (cdr records)))))
  
  (define local-table (list '*table*))
  
  (define (iter-search keylist t)
    (cond((null? t) #f)
         ((assoc (car keylist) t)
          (let ([subtable (assoc (car keylist) t)])
            (if(= 1 (length keylist))
             (cdr subtable)
             (iter-search (cdr keylist) (cdr subtable)))))
         (else #f)))
  
  (define (lookup keylist)
    (iter-search keylist (cdr local-table)))

  (define (make-subtable keylist value)
    (if(= 1 (length keylist))
       (list (cons (car keylist) value))
       (list (make-subtable (cdr keylist) value))))

  (define (iter-insert! keylist value t)
    (if(null? (cdr t))
       (set-cdr! t (make-subtable keylist value))
       (let([subtable (assoc (car keylist) (cdr t))])
         (if subtable
             (if(= 1 (length keylist))
                (set-cdr! subtable value)
                (iter-insert! (cdr keylist) subtable))
             (set-cdr! t
                       (cons (make-subtable keylist value)
                             (cdr t)))))))
  (define (insert! keylist value)
    (iter-insert! keylist value local-table))
  (define (dispatch m)
      (cond((eq? m 'lookup-proc) lookup)
           ((eq? m 'insert-proc!) insert!)
           (else
            (error "Unknown operation: TABLE" m))))
  dispatch)

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

|#


(define (make-table same-key?)
  (let ((local-table (list '*table*))) 
  
    (define (assoc key records) 
      (cond ((null? records) #false) 
            ((same-key? key (caar records)) 
             (car records)) 
            (else (assoc key (cdr records))))) 
  
    (define (lookup keys)
      (define (iter remain-keys records)
        (cond
          ((null? remain-keys) records)
          ((not (pair? records)) #false)
          (else (let ((record (assoc (car remain-keys) records)))
                  (if record
                      (iter (cdr remain-keys) (cdr record))
                      #false)))))
      (iter keys (cdr local-table)))
  
    (define (insert! keys value)
      (define (iter ks records)
        (cond
          ((null? ks) (set-cdr! records value))
          ((or (null? (cdr records)) (not (pair? (cdr records))))
           (set-cdr! records (list (cons (car ks) '()) ))
           (iter (cdr ks) (cadr records)))
          (else
           (let ((record (assoc (car ks) (cdr records))))
             (if record
                 (iter (cdr ks) record)
                 (begin (set-cdr! records
                                  (cons (list (car ks))
                                        (cdr records)))
                        (iter (cdr ks) (cadr records))))))))
      (iter keys local-table)) 
  
    (define (dispatch m) 
      (cond 
        ((eq? m 'lookup) lookup) 
        ((eq? m 'insert!) insert!))) 
    dispatch)) 
  
(define (lookup keys table) 
  ((table 'lookup) keys)) 
(define (insert! keys value table) 
  ((table 'insert!) keys value)) 

(define t (make-table equal?))

(insert! (list 1 2 3) 123 t)
(insert! (list 4 5 6) 456 t)

(lookup (list 1 2 3) t)
(lookup (list 4 5 6) t)




