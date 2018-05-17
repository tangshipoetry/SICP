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
                    (adjoin-set x (cdr set))))))

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



(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (let ([len (length pairs)])
    (if(= 1 len)
       (car pairs)
       (successive-merge (adjoin-set (make-code-tree (car pairs) (cadr pairs))
                                     (cddr pairs))))))

(define a
  '( (A 1)
     (B 2)
     (C 4)
     (D 8)
     (E 16)))

(define b
  '( (A 1)
     (B 2)
     (C 4)
     (D 8)
     (E 16)
     (F 32)
     (G 64)
     (H 128)
     (I 256)
     (J 512)))


(generate-huffman-tree a)
(generate-huffman-tree b)





#|

'(((((leaf A 1)
     (leaf B 2)
     (A B) 3)
    (leaf C 4)
    (A B C) 7)
   (leaf D 8)
   (A B C D) 15)
  (leaf E 16)
  (A B C D E)
  31)
{a b c d e} 31
                     /           \
                {a b c d} 15      e 16
                 /     \
           {a b c} 7    d 8
             /    \
        {a b} 3    c 4
         /   \
      a 1    b 2

'((((((((((leaf A 1) (leaf B 2) (A B) 3) (leaf C 4) (A B C) 7) (leaf D 8) (A B C D) 15) (leaf E 16) (A B C D E) 31)
      (leaf F 32)
      (A B C D E F)
      63)
     (leaf G 64)
     (A B C D E F G)
     127)
    (leaf H 128)
    (A B C D E F G H)
    255)
   (leaf I 256)
   (A B C D E F G H I)
   511)
  (leaf J 512)
  (A B C D E F G H I J)
  1023)
|#



















