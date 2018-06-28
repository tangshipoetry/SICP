#lang racket


;网上的

#|
; 本题和 2.42 表示方法一样,用一个list表示一种解法,比如四皇后问题,解法(A B C D)表示,第一个皇后在位置A,第二个皇后在位置B,第三个皇后在位置C,第四个皇后在位置D,用list的位置表示了皇后的次序。

; https://github.com/jiacai2050/sicp/blob/master/exercises/02/2.42_2.43.md#242


(define (check-current-diagonal? current-postion rest distance-to-current)
  (if (null? rest)
    #f
    (or (= (- current-postion distance-to-current) (car rest))
        (= (+ current-postion distance-to-current) (car rest))
        (check-current-diagonal? current-postion (cdr rest) (+ distance-to-current 1)))))

(define (in-diagonal? positions)
  (if (null? positions)
    #f
    (or (in-diagonal (car positions) (cdr positions) 1)
        (in-diagonal? (cdr positions)))))

(define (collide? positions)
  (or (not (distinct? positions))
      (in-diagonal? positions)))

(define (eight-queen)
  (let ((q1 (amb 1 2 3 4 5 6 7 8))
        (q2 (amb 1 2 3 4 5 6 7 8))
        (q3 (amb 1 2 3 4 5 6 7 8))
        (q4 (amb 1 2 3 4 5 6 7 8))
        (q5 (amb 1 2 3 4 5 6 7 8))
        (q6 (amb 1 2 3 4 5 6 7 8))
        (q7 (amb 1 2 3 4 5 6 7 8))
        (q8 (amb 1 2 3 4 5 6 7 8)))
    (require (not (collide? (list q1 q2 q3 q4 q5 q6 q7 q8))))))
|#








;; 
(define (vulnerable? queen1-position queen2-position column-separation)
  (let ((row-separation (abs (- queen1-position queen2-position))))
    (or (= row-separation 0)
        (= row-separation column-separation))))
  
;; first element of previous-queens is the position of the queen 
;; in the column immediately adjacent to next-queen 
(define (next-queen-vulnerable? next-queen previous-queens)
  (define (iter prev-qs column-separation)
    (if (null? prev-qs)
        false
        (or (vulnerable? next-queen (car prev-qs) column-separation) 
            (iter (cdr prev-qs) (1+ column-separation)))))
  (iter previous-queens 1)) 
  
;; use let* even though bindings are independent in order to guarantee efficient nesting with respect to amb. 
(define (eight-queens) 
  (define (nnqv? next-queen previous-queens) 
    (not (next-queen-vulnerable? next-queen previous-queens))) 
  (let* ((prev0 '())
         (q1 (amb 1 2 3 4 5 6 7 8)))
    (require (nnqv? q1 prev0)) ;; trivially never fails 
    (let* ((prev1 (cons q1 prev0))
           (q2 (amb 1 2 3 4 5 6 7 8)))
      (require (nnqv? q2 prev1))
      (let* ((prev2 (cons q2 prev1))
             (q3 (amb 1 2 3 4 5 6 7 8)))
        (require (nnqv? q3 prev2))
        (let* ((prev3 (cons q3 prev2))
               (q4 (amb 1 2 3 4 5 6 7 8)))
          (require (nnqv? q4 prev3))
          (let* ((prev4 (cons q4 prev3))
                 (q5 (amb 1 2 3 4 5 6 7 8)))
            (require (nnqv? q5 prev4))
            (let* ((prev5 (cons q5 prev4))
                   (q6 (amb 1 2 3 4 5 6 7 8)))
              (require (nnqv? q6 prev5))
              (let* ((prev6 (cons q6 prev5))
                     (q7 (amb 1 2 3 4 5 6 7 8)))
                (require (nnqv? q7 prev6))
                (let* ((prev7 (cons q7 prev6))
                       (q8 (amb 1 2 3 4 5 6 7 8)))
                  (require (nnqv? q8 prev7))
                  (cons q8 prev7))))))))))

























