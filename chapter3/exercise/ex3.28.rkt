#lang racket

(define (logical-or s1 s2)
  (cond((and (= 0 s1)
             (= 0 s2)) 0)
       ((or((and (= 0 s1)
                 (= 1 s2)))
           ((and (= 1 s1)
                 (= 0 s2)))
           ((and (= 1 s1)
                 (= 1 s2)))) 1)
       (else
        (error "invalid signal" s1 s2))))

(define (or-gate a1 a2 out-put)
  (define (or-action-procedure)
    (let([new-value
          (logical-or (get-signal s1) (get-signal s2))])
      (after-delay
       or-gate-delay
       (lambda()(set-signal! out-put new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)













































