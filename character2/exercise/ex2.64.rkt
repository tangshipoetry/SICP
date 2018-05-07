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
        (element-of-set? x (right-branch tree)))))n

(define (adjoin x tree)
  (cond ((null? tree)(make-tree x null null))
        ((= x (entry tree)) tree)
        ((< x (entry tree)) (adjoin x (left-branch tree)))
        ((> x (entry tree)) (adjoin x (right-branch tree)))))


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




(define e (list 1 3 5 7 9))


#|
(1 3)(5 7 9 11)             ; 分割左右子树

(5 7 9 11)                  ; 创建 1 节点
    /
   /
1(3)

   (5 7 9 11)               ; 创建 1 的左子树(空)
      /
     /
   1(3)
   /
  /
'()

    (5 7 9 11)              ; 创建 1 的右子树（包含 3）
      /
     /
    1
   / \
  /   \
'()    3

       5 (7 9 11)           ; 创建树根 5
      /
     /
    1
   / \
  /   \
'()    3

       5                    ; 创建 9 节点
      / \
     /   \
    1     9 (7 11)
   / \
  /   \
'()    3

         5                  ; 创建 9 的左子树（包含 7）
        /\
       /  \
      /    \
     /      \
    1        9 (11)
   / \      /
  /   \    /
'()    3  7

         5                  ; 创建 9 的右子树（包含 11）
        / \
       /   \
      /     \
     /       \
    1         9
   / \       / \
  /   \     /   \
'()    3   7    11

'(5
  (1 ()
     (3 ()
        ()))
  (7 ()
     (9 ()
        ())))
|#























