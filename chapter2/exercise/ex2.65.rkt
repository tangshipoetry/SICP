#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))

(define (element-of-set? x tree)
  (if(= x (entry tree))
     #t
     (if(< x (entry tree))
        (element-of-set? x (left-branch tree))
        (element-of-set? x (right-branch tree)))))

(define (adjoin x tree)
  (balance-transform (cond ((null? tree)(make-tree x null null))
                           ((= x (entry tree)) tree)
                           ((< x (entry tree)) (adjoin x (left-branch tree)))
                           ((> x (entry tree)) (adjoin x (right-branch tree))))))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if(= n 0)
     (cons '() elts)
     (let ([left-size (quotient (- n 1) 2)])
       (let([left-result
             (partial-tree elts left-size)])
         (let([left-tree (car left-result)]
              [non-left-elts (cdr left-result)]
              [right-size (- n (+ left-size 1))])
           (let([this-entry (car non-left-elts)]
                [right-result (partial-tree (cdr non-left-elts) right-size)])
             (let ([right-tree (car right-result)]
                   [remaining-elts (cdr right-result)])
               (cons (make-tree this-entry
                                left-tree
                                right-tree)
                     remaining-elts))))))))

(define (union-list set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ([x1 (car set1)][x2 (car set2)])
           (cond ((= x1 x2) (cons x1 (union-list (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-list (cdr set1) set2)))
                 ((> x1 x2) (cons x2 (union-list set1 (cdr set2)))))))))

(define (intersection-list set1 set2)
  (if(or (null? set1) (null? set2))
     null
     (let ([x1 (car set1)][x2 (car set2)])
       (cond((= x1 x2) (cons x1 (intersection-list (cdr set1) (cdr set2))))
            ((> x1 x2) (intersection-list set1 (cdr set2)))
            ((< x1 x2) (intersection-list (cdr set1) set2))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result)
    (if(null? tree)
       result
       (copy-to-list (left-branch tree)
                     (cons (entry tree)
                           (copy-to-list (right-branch tree) result)))))
  (copy-to-list tree '()))

(define (union-set set1 set2)
  (list->tree (union-list (tree->list-2 set1)
                          (tree->list-2 set2))))

(define (intersection set1 set2)
  (list->tree (intersection-list (tree->list-2 set1)
                                 (tree->list-2 set2))))






(define (balance-transform tree)
  (list->tree (tree->list-2 tree)))



























