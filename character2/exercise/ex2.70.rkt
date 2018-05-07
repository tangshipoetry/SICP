#lang racket

;树叶相关操作
;构造
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
;判断
(define (leaf? obj)
  (eq? (car obj) 'leaf))
;获取符号或权重
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

;构造节点
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))
;获取节点左子树
(define (left-branch tree)
  (car tree))
;获取节点右子树
(define (right-branch tree)
  (cadr tree))
;获取节点(树)中的符号集合——包含判断该节点是否为树叶
(define (symbols tree)
  (if(leaf? tree)
     (list (symbol-leaf tree))
     (caddr tree)))
;获取树的权重——包含判断该节点是否为树叶
(define (weight tree)
  (if(leaf? tree)
     (weight-leaf tree)
     (cadddr tree)))
;根据霍夫曼树将二进制数据解码为符号
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
;根据二进制数据当前位置判断向左或向右
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

;元素——权重表---->树叶集合,并按照从小到大顺序排列
(define (make-leaf-set pairs)
  (if(null? pairs)
     null
     (let ([pair (car pairs)])
       (adjoin-set (make-leaf (car pair)
                              (cadr pair))
                   (make-leaf-set (cdr pairs))))))

;根据霍夫曼树将原本的符号集合数据转换为对应的二进制数据
(define (encode message tree)
  (if(null? message)
     null
     (append (encode-symbol (car message) tree)
             (encode (cdr message) tree))))
;判断集合内是否有某一元素
(define (element? x set)
  (if(null? set)
     #f
     (if(eq? x (car set))
        #t
        (element? x (cdr set)))))

;将集合中单个符号转化为二进制数据,便于后续拼接
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


;将无序 符号——权重 表转化为有序树叶表
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (let ([len (length pairs)])
    (if(= 1 len)
       (car pairs)
       (successive-merge (adjoin-set (make-code-tree (car pairs) (cadr pairs))
                                     (cddr pairs))))))


(define rock-song-symbols
  '( (A 2)
     (NA 16)
     (BOOM 1)
     (SHA 3)
     (GET 2)
     (YIP 9)
     (JOB 2)
     (WAH 1)))

(define rock-song-huffman-tree
  (generate-huffman-tree rock-song-symbols))

(define a '(GET A JOB))

#|
Sha na na na na na na na na
Get a job
Sha na na na na na na na na
Wah yip yip yip yip yip yip yip yip yip
Sha boom
|#

(encode a rock-song-huffman-tree)


