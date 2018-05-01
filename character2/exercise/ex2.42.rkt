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


;空棋盘
(define empty-board null)

;在已有的格局下根据行列组合新的位置并将新的位置组合到原有格局中形成新的格局
(define (adjoin-position new-row k rest-of-queens)
   (cons (list new-row k) rest-of-queens))

;求对角线对比
(define (diagonal? p1 p2)
  (or (= (- (col p1) (row p1)) (- (col p2) (row p2)))
      (= (+ (col p1) (row p1)) (+ (col p2) (row p2)))))

;获取位置的行
(define (row position)
  (car position))

;获取位置的列
(define (col position)
  (cadr position))

(define (safe? k positions)
  (if(pair? positions)
     ()
     #t)
  )





















