#lang racket


;网上的
Eva's map can work, because he implemented it.
But when installing map in the eval as a primitive procecdure,
there is something wrong. for example:
when eval expression '(map + (1 2) (3 4)),
primitive procedure + is interpreted as '(application + env),
so the expression is (apply map (list 'application + env) (list 1 2) (list 3 4))), it doesn't work.





































