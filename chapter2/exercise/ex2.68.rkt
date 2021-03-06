#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? obj)
  (eq? (car obj) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if(leaf? tree)
     (list (symbol-leaf tree))
     (caddr tree)))

(define (weight tree)
  (if(leaf? tree)
     (weight-leaf tree)
     (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if(null? bits)
       null
       (let ([next-branch
              (choose-branch (car bits) current-branch)])
         (if(leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (decode-1 (cdr bits) tree))
            (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond((= 0 bit) (left-branch branch))
       ((= 1 bit) (right-branch branch))
       (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;将树叶按照从小到大排列
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr x set))))))

;将元素——权重表转化为树叶,并按照从小到大顺序排列
(define (make-leaf-set pairs)
  (if(null? pairs)
     null
     (let ([pair (car pairs)])
       (adjoin-set (make-leaf (car pair)
                              (cadr pair))
                   (make-leaf-set (cdr pairs))))))


(define (encode message tree)
  (if(null? message)
     null
     (append (encode-symbol (car message) tree)
             (encode (cdr message) tree))))

(define (element? x set)
  (if(null? set)
     #f
     (if(eq? x (car set))
        #t
        (element? x (cdr set)))))



(define (encode-symbol symbol tree)
  (if(leaf? tree)
     null
     (if(not (element? symbol (symbols tree)))
        (error "symbol doesn't exist in tree" symbol)
        (let ([sub-left-tree (left-branch tree)][sub-right-tree (right-branch tree)])
          (let([left-symbols (symbols sub-left-tree)][right-symbols (symbols sub-right-tree)])
            (cond ((element? symbol left-symbols)
                   (cons 0 (encode-symbol symbol sub-left-tree)))
                  ((element? symbol right-symbols)
                   (cons 1 (encode-symbol symbol sub-right-tree)))))))))



(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))



(encode '(A D A B B C A) sample-tree)















