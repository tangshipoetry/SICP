#lang racket

(job ?x ?x-job)
(job ?y ?y-job)

(rule (replace ?x ?y)
      (and (job ?x ?x-job)
           (job ?y ?y-job)
           (not (same ?x ?y))
           (or (can-do-job ?x-job ?y-job)
               (same ?x-job ?y-job))))

(replace (Cy D. Fect) ?x)

(and (replace ?x ?y)
     (salary ?x ?xs)
     (salary ?y ?ys)
     (lisp-value > ?ys ?xs))
































