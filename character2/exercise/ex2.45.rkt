#lang racket

( require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))


#|
(define (right-split painter n)
  (if(= n 0)
     painter
     (let([smaller (right-split painter (- n 1))])
       (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if(= n 0)
     painter
     (let([smaller (up-split painter (- n 1))])
       (below painter (beside smaller smaller)))))
|#

(define (split op1 op2)
  (lambda(painter n)
    (if(= 0 n)
       painter
       (let([smaller ((split op1 op2) painter (- n 1))])
         (op1 painter (op2 smaller smaller))))))


(define right-split (split beside below))
(define up-split (split below beside))


(define (corner-split painter n)
  (if(= n 0)
     painter
     (let ((up (up-split painter (- n 1)))
           (right (right-split painter (- n 1))))
       (let ((top-left (beside up up))
             (bottom-right (below right right))
             (corner (corner-split painter (- n 1))))
         (beside (below painter top-left)
                 (below bottom-right corner))))))










































