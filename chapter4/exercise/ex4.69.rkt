#lang racket


;4.63
(assert! (rule (father ?s ?f)
               (or (son ?f ?s)
                   (and (son ?w ?s)
                        (wife ?f ?w)))))
  
(assert! (rule (grandson ?g ?s) 
               (and (father ?s ?f) 
                    (father ?f ?g))))


;网上的
(rule (end-in-grandson (grandson)))
(rule (end-in-grandson (?x . ?rest))
      (end-in-grandson ?rest))
  
(rule ((grandson) ?x ?y)
      (grandson ?x ?y))
(rule ((great . ?rel) ?x ?y)
      (and (end-in-grandson ?rel
           (son ?x ?z)
           (?rel ?z ?y)))






























