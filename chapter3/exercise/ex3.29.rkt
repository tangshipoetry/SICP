#lang sicp

(define (inverter input output)
  (define (inver-input)
    (let([new-value (logical-not (get-signal input))])
      (after-delay inverter-delay
                   (lambda()(set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond((= 1 s) 0)
       ((= 0 s) 1)
       (else (error "invalid signal" s))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let([new-value
          (logical-and (set-signal! a1) (set-signal! a2))])
      (after-delay
       and-gate-delay
       (lambda()(set-signal! out-put new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)


(define (logical-and s1 s2)
  (cond((and (= 1 s1)
             (= 1 s2)) 1)
       ((or((and (= 0 s1)
                 (= 1 s2)))
           ((and (= 1 s1)
                 (= 0 s2)))
           ((and (= 0 s1)
                 (= 0 s2)) 0)))
       (else
        (error "invalid signal" s1 s2))))

(defeine (or-gate a1 a2 output)
  (let([inverter-1 (make-wire)]
       [inverter-2 (make-wire)]
       [invert-out (make-wire)])
    (inverter a1 inverter-1)
    (inverter a2 inverter-2)
    (and-gate inverter-1 inverter-2 invert-out)
    (invert invert-out output)))









































