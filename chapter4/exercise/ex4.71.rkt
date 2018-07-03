#lang racket

;书上版本
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))

;题目版本
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append
      (find-assertions query-pattern frame)
      (apply-rules query-pattern frame)))
   frame-stream))
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts)
              frame-stream)
       (disjoin (rest-disjuncts disjuncts)
                frame-stream))))



;网上的
This will postpone some infinite loop. for example: 
(assert! (married Minnie Mickey)) 
(assert! (rule (married ?x ?y) 
               (married ?y ?x))) 
(married Mickey ?who) 
if we don't use delay, there is no answer to display. but if we use it, we can get: 
;;; Query output: 
(married Mickey Minnie) 
(married Mickey Minnie) 
(married Mickey Minnie) 
.... 
this is better than nothing.
the reason of this difference is that in this example (apply-rules query-pattern frame) will lead to infinite loop,
if we delay it, we still can get some meaningful answers. 








































