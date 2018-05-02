#lang racket



;过滤器
(define (filter p sequence)
  (if(null? sequence)
     '()
     (if(p (car sequence))
        (cons (car sequence)
              (filter p (cdr sequence)))
        (filter p (cdr sequence)))))

;interval enumerate
(define (enumerate-interval lower high)
  (if(> lower high)
     null
     (cons lower
           (enumerate-interval (+ 1 lower)
                               high))))

;操作累计
(define (accumulate op initial sequence)
  (if(null? sequence)
     initial
     (op (car sequence)
         (accumulate op initial (cdr sequence)))))


(define (flatmap proc seq)
  (accumulate append
              null
              (map proc seq)))






;空棋盘
(define empty-board null)

;在已有的格局下根据行列组合新的位置并将新的位置组合到原有格局中形成新的格局
(define (adjoin-position new-row k rest-of-queens)
   (cons (list new-row k) rest-of-queens))

;判断两个棋子是否在同一个对角线
(define (diagonal? p1 p2)
  (or (= (- (row p1) (col p1)) (- (row p2) (col p2)))
      (= (+ (row p1) (col p1)) (+ (row p2) (col p2)))))

;获取位置的行
(define (row position)
  (car position))

;获取位置的列
(define (col position)
  (cadr position))

(define (safe? k positions)
  (let ([new-queen (car positions)])
    (define (iter rq)
      (if(pair? rq)
         (let ([current-queen (car rq)])
           (if(or (= (col new-queen) (col current-queen))
                  (= (row new-queen) (row current-queen))
                  (diagonal? new-queen current-queen))
              #f
              (iter (cdr rq))))
         #t))
    (iter (cdr positions))))







#|
(define (queens board-size)
  (define (queen-cols k);在前K-1列中已经安全存放棋子的情况下,在第K列安全放置棋子
    (if(= k 0)
       (list empty-board);空棋盘
       (filter
        (lambda(positions)(safe? k positions)) ;过滤出安全的位置组合
        (flatmap
         (lambda(rest-of-queens)
           (map (lambda(new-row)
                  (adjoin-position new-row k rest-of-queens))
                (enumerate-interval 1 board-size)))
         (queen-cols (- k 1))))))
  (queen-cols board-size))
|#


#|
;摘抄
Exchanging the order of the mapping in the flatmap results in queen-cols being re-evaluated for every item in (enumerate-interval 1 board-size).
 Therefore the whole work has to be duplicated board-size times at every recursion level.
 Since there are always board-size recursions this means that the whole work will be duplicated bord-size^board-size times.
Therefore if the function would take T time to run for board-size=8 with correct ordering, with the interchanged ordering it will take (8^8)T to run.
|#

#|
;摘抄
正常的解的内层循环代价很小,但是Louis的解的内层是递归调用,所以代价很大! (queen-cols k) 需要调用n次 (queen-cols (- k 1)) ,而 (queen-cols (- k 1))
又需要调用n次 (queen-cols (- k 2)) ,如此递归下去,可以推出: (queen-cols k) 最终调用了n的n次方次 (queen-cols 0) (因为到这一级才是常数级调用,所以要一直展开到这一级)
所以，如果正常解的时间为T的话，那么Louis的解针对8x8的棋盘就是 8^8 * T 。。
|#

(define (queens board-size)
  (define (queen-cols k);在前K-1列中已经安全存放棋子的情况下,在第K列安全放置棋子
    (if(= k 0)
       (list empty-board);空棋盘
       (filter
        (lambda(positions)(safe? k positions)) ;过滤出安全的位置组合
        (flatmap
         (lambda(new-row)
           (map (lambda(rest-of-queens)
                  (adjoin-position new-row k rest-of-queens))
                (queen-cols (- k 1))))
         (enumerate-interval 1 board-size)))))
  (queen-cols board-size))








































