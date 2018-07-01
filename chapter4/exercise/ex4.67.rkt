#lang racket


;网上的，还没看

Since we need to prevent rules being applied to the same query pattern it makes sense to add a query pattern history.
To do this, we instantiate the query pattern and any duplicates result in an empty stream of frames in response.

In the query evaluator add:




;;; Loop detection history
(define history (make-hash))
(define (reset-history!)
  (set! history (make-hash)))
(define (history-ref key)
  (hash-ref history key #f))
(define (history-add! key)
  (hash-set! history key #t))
;; Get the canonical name for a variable
;; when rules are applied new variables are generated - see:
;;    apply-a-rule -&gt; rename-variable-in -&gt; make-new-variable
;; so ?who becomes the variable (? who) which becomes (? 1 who) then (? 2 who) ...
;; the canonical name is (? who)
(define (canonical-name var)
  (if (number? (mcadr var))
      (mlist (mcar var) (mcaddr var))
      var))



Change apply-a-rule:

(define (apply-a-rule rule query-pattern query-frame)
  (let* [(clean-rule (rename-variables-in rule))
         (unify-result (unify-match query-pattern
                                    (conclusion clean-rule)
                                    query-frame))]
      (if (eq? unify-result 'failed)
          the-empty-stream
          (let ([instance (instantiate query-pattern
                            query-frame
                            (lambda (var frame)
                              (canonical-name var)))])
            (if (history-ref instance)
                (loop-detected instance)
                (begin
                  (history-add! instance)
                  (qeval (rule-body clean-rule)
                         (singleton-stream unify-result))))))))



Also add a call to reset the query pattern history to the driver loop.
I added a convenient interpret-query method to save typing into the driver loop which needs the call too.


(define (interpret-query query)
  (reset-history!)
  (let ((q (query-syntax-process query)))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display &quot;Assertion added to data base.&quot;))
          (else
           (newline)
           ; (display-stream
           (stream-&gt;list
            (stream-map
             (lambda (frame)
               (mlist-&gt;exp (instantiate q
                             frame
                             (lambda (v f)
                               (contract-question-mark v)))))
             (qeval q (singleton-stream '()))))))))

Time to test outranked-by and Louis’ version outranked-by-loop


(run-query
   '(outranked-by ?person ?who))
 
'((outranked-by (Aull DeWitt) (Warbucks Oliver))
  (outranked-by (Cratchet Robert) (Warbucks Oliver))
  (outranked-by (Cratchet Robert) (Scrooge Eben))
  (outranked-by (Reasoner Louis) (Bitdiddle Ben))
  (outranked-by (Scrooge Eben) (Warbucks Oliver))
  (outranked-by (Reasoner Louis) (Warbucks Oliver))
  (outranked-by (Bitdiddle Ben) (Warbucks Oliver))
  (outranked-by (Reasoner Louis) (Hacker Alyssa P))
  (outranked-by (Tweakit Lem E) (Bitdiddle Ben))
  (outranked-by (Fect Cy D) (Bitdiddle Ben))
  (outranked-by (Hacker Alyssa P) (Bitdiddle Ben)))
 
;; Louis Reasoner version which couldn't be run without the loop detector.
(run-query
 '(assert!
   (rule (outranked-by-loop ?staff-person ?boss)
         (or (supervisor ?staff-person ?boss)
             (and (outranked-by-loop ?middle-manager ?boss)
                  (supervisor ?staff-person ?middle-manager))))))
 
'((outranked-by-loop (Aull DeWitt) (Warbucks Oliver))
  (outranked-by-loop (Cratchet Robert) (Warbucks Oliver))
  (outranked-by-loop (Cratchet Robert) (Scrooge Eben))
  (outranked-by-loop (Tweakit Lem E) (Warbucks Oliver))
  (outranked-by-loop (Scrooge Eben) (Warbucks Oliver))
  (outranked-by-loop (Reasoner Louis) (Bitdiddle Ben))
  (outranked-by-loop (Bitdiddle Ben) (Warbucks Oliver))
  (outranked-by-loop (Fect Cy D) (Warbucks Oliver))
  (outranked-by-loop (Reasoner Louis) (Hacker Alyssa P))
  (outranked-by-loop (Hacker Alyssa P) (Warbucks Oliver))
  (outranked-by-loop (Tweakit Lem E) (Bitdiddle Ben))
  (outranked-by-loop (Fect Cy D) (Bitdiddle Ben))
  (outranked-by-loop (Hacker Alyssa P) (Bitdiddle Ben)))


Finally what about Mickey?
(run-query
 '(assert! (married Minnie Mickey)))
(run-query
 '(assert! (rule (married ?x ?y)
                 (married ?y ?x))))
 
(run-query '(married Mickey ?who))
'((married Mickey Minnie) (married Mickey Minnie))





