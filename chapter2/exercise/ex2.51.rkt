#lang racket
( require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))

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


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (counterroute270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0) ; new origin
                     (make-vect 0.0 0.0) ; new end of edge1
                     (make-vect 1.0 1.0))) ; new end of edge2

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

#|
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))
|#

#|
;模仿beside
(define (below painter1 painter2)
  (let (split-point (make-vect 0.0 0.5))
    (let ([painter-below
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point)]
          [painter-top
           (transform-painter
            painter1
            split-point
            (make-vect 0.0 1.0)
            (make-vect 1.0 0.5))]))
    (lambda(frame)
      (painter-below frame)
      (painter-top frame))))
|#

(define (top painter1 painter2)
  (lambda(frame)
    ((counterroute270
     (beside
      (rotate90 painter1)
      (rotate90 painter2)))
     frame)))




































