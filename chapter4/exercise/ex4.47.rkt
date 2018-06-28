#lang racket


(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

;网上的
#|
 This doesn't work.
Because the second branch of amb expression will call (parse-verb-phrase) again,
this will lead to infinite loop.
If we change the order in amb, it still will lead to infinite loop. 
|#




































