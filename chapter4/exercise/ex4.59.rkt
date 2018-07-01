#lang racket


(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))

(meeting whole-company (Wednesday 4pm))

;a）
(meeting ?any (Friday . ?time))



(rule (meeting-time ?person ?day-and-time)
      (or (and (job ?person (?division . ?x))
               (meeting ?division ?day-and-time))
          (meeting whole-company ?day-and-time)))


(meeting-time Alyssa (Wednesday . ?time))


























