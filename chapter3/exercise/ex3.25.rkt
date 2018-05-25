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


#|

|#

;网上参考
(define (make-table)
  (define local-table (list '*table*))
  (define (assoc key records)
    (cond ((null? records) #false)
          ((equal? key (caar records))
           (car records))
          (else (assoc key (cdr records)))))
  (define (lookup keys)
    (define (iter keys remainder-records)
      (cond((null? keys) remainder-records)
           ((not (pair? remainder-records)) #f)
           (else
            (let ([subtable (assoc (car keys) remainder-records)])
              (if subtable
                  (iter (cdr keys) (cdr subtable))
                  #f)))))
    (iter keys (cdr local-table)))
  (define (insert! keys value)
    (define (iter keys table)
      (cond((null? keys) (set-cdr! table value))
           ((not (pair? (cdr table)))
            (set-cdr! table (list (list (car keys))))
            (iter (cdr keys) (cadr table)))
           (else (let ([subtable (assoc (car keys) (cdr table))])
                   (if subtable
                       (iter (cdr keys) subtable)
                       (begin (set-cdr! table (cons (list (car keys))
                                                    (cdr table)))
                              (iter (cdr keys) (cadr table))))))))
    (iter keys local-table))
  (define (dispatch m) 
    (cond 
      ((eq? m 'lookup) lookup) 
      ((eq? m 'insert!) insert!))) 
    dispatch)


(define (lookup keys table) 
  ((table 'lookup) keys)) 
(define (insert! keys value table) 
  ((table 'insert!) keys value))

(define t (make-table))

(insert! (list 1 2 3) 123 t)
(insert! (list 4 5 6) 456 t)

(lookup (list 1 2 3) t)
(lookup (list 4 5 6) t)
(insert! (list 1 2 3 7) "test" t)
(lookup (list 1 2 3 7) t)

#|
;网上参考,有bug
(define (insert! key-list value table)
  (if (list? key-list)
      (let ((current-key (car key-list))
            (remain-key (cdr key-list)))
        (let ((record (assoc current-key (cdr table))))
          (cond 
            ; 1) 有记录,且没有其他关键字
            ;    更新记录的值
            ((and record (null? remain-key))
             (set-cdr! record value)
             table)
            ; 2) 有记录,且还有其他关键字
            ;    说明这个记录实际上是一个子表
            ;    使用 insert! 递归地进行插入操作
            ((and record remain-key)
             (insert! remain-key value record)
             table)
            ; 3) 无记录,且有其他关键字
            ;    需要执行以下三步:
            ;    一、 创建子表
            ;    二、 对子表进行插入
            ;    三、 将子表加入到 table
            ;    这三个步骤可以用一句完成,wow!
            ((and (not record) (not (null? remain-key)))
             (join-in-table (insert! remain-key value (make-table current-key)) table)
             table)
            ; 4) 无记录,且无其他关键字
            ;    创建记录并将它加入到 table
            ((and (not record) (null? remain-key))
             (let ((new-record (cons current-key value)))
               (join-in-table new-record table)
               table)))))
      (insert! (list key-list) value table)))  ; 将单个键转换成列表

(define (join-in-table new-record table)
  (set-cdr! table
            (cons new-record (cdr table))))

(define (lookup key-list table)
  (if (list? key-list)
      (let ((current-key (car key-list))
            (remain-key (cdr key-list)))
        (let ((record (assoc current-key (cdr table))))
          (if record
              (if (null? remain-key)
                  (cdr record)
                  (lookup remain-key record))
              #f)))
      (lookup (list key-list) table)))    ; 将单个键转换成列表

(define (make-table . table-name) 
  (if (null? table-name)
      (list '*table*)
      table-name))

; p 184
(define (assoc key records)
  (cond ((null? records)
         #f)
        ((equal? key (caar records))
         (car records))
        (else
         (assoc key (cdr records)))))



(define t (make-table))
(insert! (list 1 2 3) 123 t)
(insert! (list 4 5 6) 456 t)
(lookup (list 1 2 3) t)
(lookup (list 4 5 6) t)

(insert! (list 1 2 3 7) "test" t);出bug了
|#















