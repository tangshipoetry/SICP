#lang racket/gui
(require graphics/graphics)

( require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))


(open-graphics)
(define vp-width 500)
(define vp-height 500)
(define vp (open-viewport "A Picture Language" vp-width vp-height))
(define draw (draw-viewport vp))
(define clear (clear-viewport vp))
(define line (draw-line vp))



(define (make-vect x y)
  (cons x y))

(define (xcor-vect vect)
  (car vect))

(define (ycor-vect vect)
  (cdr vect))

(define (add-vect  vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
             (+ (ycor-vect vect1) (ycor-vect vect2))))


(define (sub-vect  vect1 vect2)
  (make-vect (- (xcor-vect vect1) (xcor-vect vect2))
             (- (ycor-vect vect1) (ycor-vect vect2))))


(define (scale-vect s vect)
  (make-vect (* s (xcor-vect vect))
             (* s (ycor-vect vect))))




(define (make-segment start end)
  (list start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))



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





(define ori (make-vect 0 0))
(define x-unit (make-vect 1 0))
(define y-unit (make-vect 0 1))
(define diag (make-vect 1 1))


(define x-coor-unit (make-segment ori x-unit))
(define y-coor-unit (make-segment ori y-unit))
(define op-x-coor-unit (make-segment y-unit diag))
(define op-y-coor-unit (make-segment x-unit diag))

;frame边界
(define outline (list x-coor-unit x-coor-unit op-x-coor-unit op-y-coor-unit))

(define rt->lb (make-segment diag ori))

(define lt->rb (make-segment y-unit x-unit))

;对角线
(define diagonal-line (list rt->lb lt->rb))

;
(define a (scale-vect 0.5 x-unit)) ;(0.5  0)
(define b (scale-vect 0.5 y-unit)) ;(0  0.5)
(define c (make-vect 0.5 1))
(define d (make-vect 1 0.5))








































