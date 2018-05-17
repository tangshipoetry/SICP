#lang racket

;构造活动体
(define (make-mobile left right)
  (list left right))
;构造分支
(define (make-branch length structure)
  (list length structure))

;获取活动体左分支
(define (left-branch binary-mobile)
  (car binary-mobile))
;获取活动体右分支
(define (right-branch binary-mobile)
  (cadr binary-mobile))

;获取分支长度
(define (branch-length branch)
  (car branch))
;获取分支中重量或活动体部分
(define (branch-structure branch)
  (cadr branch))


(define (branch-weight branch)
  (if(null? branch)
     0
     (let((ws (branch-structure branch)))
       (if(pair? ws)
          (+ (branch-weight (left-branch ws))
             (branch-weight (right-branch ws)))
          ws))))

#|
;活动体总重量--直接构造
(define (total-weight binary-mobile)
  (if(null? binary-mobile)
     0
     (if(pair? binary-mobile)
        (+ (total-weight (branch-structure (left-branch binary-mobile)))
           (total-weight (branch-structure (right-branch binary-mobile))))
        binary-mobile)))
|#


;活动体总重量--分支构造
(define (total-weight binary-mobile)
  (if(null? binary-mobile)
     0
     (if(pair? binary-mobile)
        (+ (branch-weight (left-branch binary-mobile))
           (branch-weight (right-branch binary-mobile)))
        binary-mobile)))


;力矩
(define (torque branch)
  (if(null? branch)
     0
     (if(pair? branch)
        (* (branch-weight branch)
           (branch-length branch))
        (error "expected a branch"))))


(define (balance? moblie)
  (if(pair? moblie)
     (and (= (torque (left-branch moblie))
             (torque (right-branch moblie)))
          (balance? (branch-structure (left-branch moblie)))
          (balance? (branch-structure (right-branch moblie))))
     #t))










(define a (make-branch 3 5))
(define b (make-branch 2 6))
(define c (make-mobile a b))
(define d (make-branch 7 3))
(define e (make-branch 4 5))
(define f (make-mobile d e))

(define branch1 (make-branch 3 c))
(define branch2 (make-branch 4 f))

(define m (make-mobile branch1 branch2))

(total-weight m)

(balance? m)


































