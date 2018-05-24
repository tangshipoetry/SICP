#lang sicp
#|
(define (make-table)
  (let([local-table (list '*table*)])
    (define (lookup key1 key2)
      (let([subtable
            (assoc key1 (cdr local-table))])
        (if subtable
            (let([record
                  (assoc key2 (cdr subtable))])
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key1 key2 value)
      (let([subtable (assoc key1 (cdr local-table))])
        (if subtable
            (let([record
                  (assoc key2 (cdr subtable))])
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key1 (cons key2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond((eq? m 'lookup-proc) lookup)
           ((eq? m 'insert-proc!) insert!)
           (else
            (error "Unknown operation: TABLE" m))))
    dispatch))
|#
(define (assoc key records)
      (cond((null? records) #f)
           ((equal? key (caar records)) (car records))
           (else (assoc key (cdr records)))))


(define (make-list)
  (define local-table (list '*table*))
  (define (iter-search keylist t)
    (cond((null? t) #f)
         ((assoc (car keylist) t)
          (let ([subtable (assoc (car keylist) t)])
            (if(= 1 keylist)
             (cdr subtable)
             (iter-search (cdr keylist) subtable))))
         (else #f)))
  (define (lookup keylist)
    (iter-search keylist local-table))

  (define (iter-insert! keylist value t)
    (cond((null? t) ())
         (()))))


























