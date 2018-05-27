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


;自己写的
(define (ripple-carry-adder list-a list-b list-s c)
  (if(and (null? list-a) (null? list-b))
     'ok
     (let([a (car list-a)]
          [b (car list-b)]
          [s (car list-s)])
       (full-adder a b c s c)
       (ripple-carry-adder (cdr list-a) (cdr list-b) (cdr list-s) c))))


;网上摘抄
(define (ripple-carry-adder list-A list-B list-S C)
  (define (iter A B S value-of-c)
    (if (and (null? A) (null? B) (null? S))
        'ok
        (let ((Ak (car A))
              (Bk (car B))
              (Sk (car S))
              (remain-A (cdr A))
              (remain-B (cdr B))
              (remain-S (cdr S))
              (Ck (make-wire)))
          (set-signal! Ck value-of-c)
          (full-adder Ak Bk Ck Sk C)
          (iter remain-A remain-B remain-S (get-signal C)))))
  (iter list-A list-B list-S (get-signal C)))

;网上摘抄
(define (ripple-carry-adder a-list b-list s-list c)
  (let ((c-list (map (lambda (x) (make-wire)) (cdr a-list)))
        (c-0 (make-wire)))
    (map full-adder
         a-list
         b-list
         (append c-list (list c-0))
         s-list
         (cons c c-list))
    'ok))





























