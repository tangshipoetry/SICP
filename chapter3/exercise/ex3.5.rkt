#lang sicp

(define (square x)(* x x))
(define (min x y)
  (if(> x y)
     y
     x))
(define (average x y)
  (/ (+ x y) 2.0))

(define (random-in-range low high)
  (let([range (- high low)])
    (+ low (random range))))

(define (mento-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (if(= 0 trials-remaining)
       (/ trials-passed trials)
       (if(experiment)
          (iter (- trials-remaining 1) (+ trials-passed 1))
          (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p? x1 x2 y1 y2 trials)
  (define cen-x (average x1 x2))
  (define cen-y (average y1 y2))
  (define c (abs (/ (- x2 x1) 2.0)))
  (define (experiment)(p? cen-x cen-y c x1 x2 y1 y2))
  (* 4.0 (* (mento-carlo trials experiment))))




(define (in-region? cen-x cen-y c x1 x2 y1 y2)
  (let([x (random-in-range x1 x2)]
       [y (random-in-range y1 y2)])
    (<= (+ (square (- x cen-x)) (square (- y cen-y)))
        (square c))))


(estimate-integral in-region? 1.0 10.0 1.0 10.0 10000000)






