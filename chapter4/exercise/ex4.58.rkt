#lang racket

(rule (big-shot ?person ?division)
      (and (job ?person (?division . ?type))
           (or (not (supervisor ?person ?boss))
               (and (supervisor ?person ?boss)
                    (not ?boss (?division . ?any))))))




































