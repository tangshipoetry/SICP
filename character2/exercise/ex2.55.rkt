#lang racket


(car ''a)

#|
(car ''a)
 |
\|/
(car (quote (quote a)))

(car (list 'quote (list 'quote a)))



|#






























