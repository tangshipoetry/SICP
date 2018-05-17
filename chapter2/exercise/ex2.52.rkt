#lang racket

( require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))

;a:直接在segments->painter的segment-list中添加线段

#|
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

|#





;(paint einstein)

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

(define (corner-split painter n)
  (if(= n 0)
     painter
     (let ((up (up-split painter (- n 1)))
           (right (right-split painter (- n 1))))
       (let ((top-left up)
             (bottom-right right)
             (corner (corner-split painter (- n 1))))
         (beside (below painter top-left)
                 (below bottom-right corner))))))


;(paint (corner-split einstein 2))

;原版，暂不修改
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))






























