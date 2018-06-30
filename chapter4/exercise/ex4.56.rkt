#lang racket

(and (supervisor ?x (Ben Bitdiddle))
     (address ?x ?y))

(and (salary (Ben Bitdiddle) ?x)
     (salary ?person ?y)
     (lisp-value > ?x ?y))

(and (job ?person ?job)
     (supervisor ?person ?supervisor)
     (not (job ?supervisor (computer . ?x))))




















