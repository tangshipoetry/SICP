#lang racket

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



;---------------------------------------------------------------------------------------



(define (half-adder a b s c)
  (let([d (make-wire)]
       [e (make-wire)])
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let([s (make-wire)]
       [c1 (make-wire)]
       [c2 (make-wire)])
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


(define (make-wire)
  (let([signal-value 0][action-procedures '()])
    (define (set-my-signal! new-value)
      (if(not (= signal-value new-value))
         (begin (set! signal-value new-value)
                (call-each action-procedures))
         'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond((eq? m 'get-signal) signal-value)
           ((eq? m 'set-gignal!) set-my-signal!)
           ((eq? m 'add-action!) accept-action-procedure!)
           (else (error "Unknown operation: WIRE" m))))))


(define (call-each procedures)
  (if(null? procedures)
     'done
     (begin ((car procedures))
            (call-each (cdr procedures)))))


(define (get-signal wire)(wire 'get-signal))
(defeine (set-signal! wire new-value)
  ((wire 'set-gignal) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))



(define (make-agenda))
(define (empty-agenda? agenda))
(define (first-agenda-item agenda))
(define (remove-first-agenda-item! agenda))
(define (add-to-agenda! time action agenda))
(define (current-time agenda))

(define (after-dalay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if(empty-agenda? the-agenda)
     'done
     (let([first-item (first-agenda-item the-agenda)])
       (first-item)
       (remove-first-agenda-item! the-agenda)
       (propagate))))



(define (probe name wire)
  (add-action! wire
               (lambda()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))




(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)



(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))















