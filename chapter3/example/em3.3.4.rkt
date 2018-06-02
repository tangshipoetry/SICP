#lang sicp

(define (make-queue) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (front-queue queue)
  (if(empty-queue? queue)
     (error "FRONT called with an empty queue" queue)
     (car (front-ptr queue))))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))

(define (insert-queue! queue item)
  (let([new-pair (list item)])
    (cond((empty-queue? queue)
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
         (else
          (set-cdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))

(define (delete-queue! queue)
  (cond((empty-queue? queue)
        (error "FRONT called with an empty queue" queue))
       (else
        (set-front-ptr! queue (cdr (front-ptr queue)))
        queue)))

(define (print-queue queue)
  (car queue))

;------------------------------------------------------------------------------------------------------------------------------------------------------
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s)(car s))
(define (segment-queue s)(cdr s))

(define (make-agenda)
  (list 0))
(define (current-time agenda)
  (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda)
  (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
;----------------------------------------------------------------------------------------------------------------------------------------------------------
(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let([q (make-queue)])
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if(= (segment-time (car segments)) time)
       (insert-queue! (segment-time (car segments))
                      action)
       (let([rest (cdr segments)])
         (if(belongs-before? rest)
            (set-cdr! segments (cons (make-new-time-segment time action)
                                     (cdr segments)))
            (add-to-segments! rest)))))
  (let([segments (segments agenda)])
    (if(belongs-before? segments)
       (set-segments! agenda (cons (make-new-time-segment time action)
                                  segments))
       (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let([q (segment-queue (first-segment agenda))])
    (delete-queue! q)
    (if(empty-queue? q)
       (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
;----------------------------------------------------------------------------------------------------------------------------------------------------------

(define (call-each procedures)
  (if(null? procedures)
     'done
     (begin ((car procedures))
            (call-each (cdr procedures)))))

(define (make-wire)
  (let([signal-value 0][action-procedures '()])
    ;信号设置，若与当前信号不一样,更改当前信号,并依次全部调用储存在线路中过程
    (define (set-my-signal! new-value)
      (if(not (= signal-value new-value))
         (begin (set! signal-value new-value)
                (call-each action-procedures))
         'done))
    ;线路内功能过程,在信号改变时依次调用。添加新功能时,立即运行这个新功能
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    ;按输入进行功能分配
    (define (dispatch m)
      (cond((eq? m 'get-signal) signal-value)
           ((eq? m 'set-gignal!) set-my-signal!)
           ((eq? m 'add-action!) accept-action-procedure!)
           (else (error "Unknown operation: WIRE" m))))
    dispatch))
;小结:对于一般的wire,一般只会储存一个功能函数,这种情况下,在功能添加和信号修改时,过程调用


(define (get-signal wire)(wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-gignal) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;------------------------------------------------------------------------------------------------------------------------------------------------------
(define (inverter input output)
  (define (invert-input)
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
          (logical-and (get-signal a1) (get-signal a2))])
      (after-delay
       and-gate-delay
       (lambda()(set-signal! output new-value)))))
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



;----------------------------------------------------------------------------------------------------------------------------------------------------------
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
          (logical-or (get-signal a1) (get-signal a2))])
      (after-delay
       or-gate-delay
       (lambda()(set-signal! out-put new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)



;----------------------------------------------------------------------------------------------------------------------------------------------------------

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

;----------------------------------------------------------------------------------------------------------------------------------------------------------
(define (after-delay delay action)
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

;----------------------------------------------------------------------------------------------------------------------------------------------------------

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

































