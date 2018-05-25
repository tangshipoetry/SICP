#lang sicp

#|

;参照网上
;比较
(define (compare-string x y)
  (cond ((string=? x y)
         0)
        ((string>? x y)
         1)
        ((string<? x y)
         -1)))

(define (compare-symbol x y)
  (compare-string (symbol->string x)
                  (symbol->string y)))

(define (compare-number x y)
  (cond ((= x y)
         0)
        ((> x y)
         1)
        ((< x y)
         -1)))

;选择器
(define (make-tree key value left-branch right-branch)
  (list make-tree key value left-branch right-branch))

(define (tree-key tree)
  (car tree))

(define (tree-value tree)
  (cadr tree))

(define (tree-left-branch tree)
  (caddr tree))

(define (tree-right-branch tree)
  (cadddr tree))

(define (tree-empty? tree)
  (null? tree))

;修改器
(define (tree-set-key! key tree)
  (set-car! tree key))

(define (tree-set-value! value tree)
  (set-car! (cdr tree) value))

(define (tree-set-left-branch! new-left tree)
  (set-car! (cddr tree) new-left))

(define (tree-set-rigt-branch! new-right tree)
  (set-car! (cddr tree) new-right))



(define (tree-insert! tree given-key value compare)
  (if(tree-empty? tree)
     (make-tree given-key value nil nil)
     (let([compare-result (compare given-key (tree-key tree))])
       (cond((= 0  compare-result)
             (tree-set-value! value tree))
            ((= 1  compare-result)
             (tree-set-rigt-branch! (tree-insert!
                                     (tree-right-branch tree)
                                     given-key
                                     value
                                     compare)
                                    tree)
             tree)
            ((= -1 compare-result)
             (tree-set-left-branch! (tree-insert!
                                     (tree-left-branch tree)
                                     given-key
                                     value
                                     compare)
                                    tree)
             tree)))))



(define (tree-search! tree given-key compare)
  (if(tree-empty? tree)
     nil
     (let([compare-result (compare given-key (tree-key tree))])
       (cond((= 0 compare-result)
             tree)
            ((= 1 compare-result)
             (tree-search! (tree-right-branch tree) given-key compare))
            ((= -1 compare-result)
             (tree-search! (tree-left-branch tree) given-key compare))))))



(define (make-table compare)
  (let([t '()])
    (define (empty?)
      (tree-empty? t))
    (define (insert! given-key value)
      (set! t (tree-insert! t given-key value compare))
      'ok)
    (define (lookup given-key)
      (let([result (tree-search! t given-key compare)])
        (if(null? result)
           #f
           (tree-value t))))
    (define (dispatch m)
            (cond ((eq? m 'insert!)
                    insert!)
                  ((eq? m 'lookup)
                    lookup)
                  ((eq? m 'empty?)
                    (empty?))
                  (else
                    (error "Unknow mode " m))))
    dispatch))

|#


(define (make-record key value) 
  (list (cons key value) nil nil)) 
(define (get-key record) (caar record)) 
(define (get-value record) (cdar record)) 
(define (set-key! record new-key) (set-car! (car record) new-key)) 
(define (set-value! record new-value) (set-cdr! (car record) new-value)) 
(define (get-left record) (cadr record)) 
(define (get-right record) (caddr record)) 
(define (set-left! record new-left) (set-car! (cdr record) new-left)) 
(define (set-right! record new-right) (set-car! (cddr record) new-right)) 
  
(define (assoc key records) 
  (cond ((null? records) false) 
        ((equal? key (get-key records)) (get-value records)) 
        ((< key (get-key records)) (assoc key (get-left records))) 
        (else (assoc key (get-right records))))) 
  
(define (add-record key value table) 
  (define (iter record parent set-action) 
    (cond ((null? record) (let ((new (make-record key value))) 
                            (set-action parent new) 
                            (car new))) 
          ((equal? key (get-key record)) (set-value! record value) 
                                         (car record)) 
          ((< key (get-key record)) (iter (get-left record) record set-left!)) 
          (else (iter (get-right record) record set-right!)))) 
  (iter (cdr table) table set-cdr!)) 
  
; the procedure 
  
(define (make-table) 
  
  (let ((local-table (list '*table*))) 
  
    (define (lookup keys) 
      (define (iter keys records) 
        (if (null? keys) records 
            (let ((found (assoc (car keys) records))) 
              (if found (iter (cdr keys) found) 
                  false)))) 
      (iter keys (cdr local-table))) 
  
    (define (insert! keys value) 
      (define (iter keys subtable) 
        (cond ((null? (cdr keys)) (add-record (car keys) value subtable)) 
              (else (let ((new (add-record (car keys) nil subtable))) 
                      (iter (cdr keys) new))))) 
      (iter keys local-table) 
      'ok) 
  
    (define (print) (display local-table) (newline)) 
  
    (define (dispatch m) 
      (cond ((eq? m 'lookup-proc) lookup) 
            ((eq? m 'insert-proc!) insert!) 
            ((eq? m 'print) print) 
            (error "Unknown operation - TABLE" m))) 
    dispatch)) 
  
(define operation-table (make-table)) 
(define get (operation-table 'lookup-proc)) 
(define put (operation-table 'insert-proc!)) 
(define print-table (operation-table 'print)) 














