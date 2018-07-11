#lang racket

(f 'x 'y)
((f) 'x 'y)
(f (g 'x) y)
(f (g 'x) 'y)


;网上的
(f 'x 'y)     => all the saves and restores are superfluous. because they doesn't change the registers. 
((f) 'x 'y)   => all the saves and restores are superfluous. 
(f (g 'x) y) => register proc, argl will needed save and restore. 
(f (g 'x) 'y) => same to the above one. 






























