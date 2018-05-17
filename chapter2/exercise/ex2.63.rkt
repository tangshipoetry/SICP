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
  (cond ((null? tree)(make-tree x null null))
        ((= x (entry tree)) tree)
        ((< x (entry tree)) (adjoin x (left-branch tree)))
        ((> x (entry tree)) (adjoin x (right-branch tree)))))



(define (tree->list-1 tree)
  (if(null? tree)
     null
     (append (tree->list-1 (left-branch tree))
             (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result)
    (if(null? tree)
       result
       (copy-to-list (left-branch tree)
                     (cons (entry tree)
                           (copy-to-list (right-branch tree) result)))))
  (copy-to-list tree '()))


(define a (make-tree 7
                     (make-tree 3
                                (make-tree 1 '() '())
                                (make-tree 5 '() '()))
                     (make-tree 9
                                '()
                                (make-tree 11 '() '()))))


(define b (make-tree 3
                     (make-tree 1 '() '())
                     (make-tree 7
                                (make-tree 5 '() '())
                                (make-tree 9
                                           '()
                                           (make-tree 11 '() '())))))

(define c (make-tree 5
                     (make-tree 3
                                (make-tree 1 '() '())
                                '())
                     (make-tree 9
                                (make-tree 7 '() '())
                                (make-tree 11 '() '()))))





(tree->list-1 a)
(tree->list-1 b)
(tree->list-1 c)

(tree->list-2 a)
(tree->list-2 b)
(tree->list-2 c)





































