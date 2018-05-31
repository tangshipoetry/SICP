#lang sicp

;用特定过程调用除特定项之外的其他约束列表中的各项
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond((null? items) 'done)
         ((eq? (car items) exception) (loop (cdr items)))
         (else (procedure (car items))
               (loop (cdr items)))))
  (loop list))

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;构造连接器
(define (make-connector)
  ;该连接器的值,该连接器值的设置方,该连接器所连的约束
  (let([value false][informant false][constraints '()])
    (define (set-my-value newval setter)
      (cond ((not (has-value? me));该连接原本无值,则设置新值与设置方
             (set! value newval)
             (set! informant setter)
             (for-each-except setter;通知与该连接相连的所有约束,此时该连接有了一个新值,约束调用自己的内部过程,进行约束相关的其他连接的值的设置
                              inform-about-value
                              constraints))
            ((not (= value newval));连接有值且与此时想要设置的值不一样,报错——需要进行forget操作
             (error "Contradiction" (list value newval)))
            (else 'ignored)));新值与旧值相等,不进行操作
    ;连接撤销操作
    (define (forget-my-value retractor)
      (if(eq? retractor informant);在撤销方与设置方一致时,撤销原值，并通知与该连接相连的所有约束该撤销操作
         (begin (set! informant false)
                (for-each-except retractor
                                 inform-about-no-value
                                 constraints))
         'ignored));在撤销方与设置方不一致时,忽略该操作
    ;设置当前连接与新的约束相连
    (define (connect new-constraint)
      (if(not (memq new-constraint constraints));该约束不存在于当前连接的约束列表中时,则进行添加
         (set! constraints
               (cons new-constraint constraints)))
      (if(has-value? me)
         (inform-about-value new-constraint));当前连接有值时,通知该新约束,进行约束传播
      'done)
    (define (me request)
      (cond((eq? request 'has-value?)
            (if informant true false))
           ((eq? request 'value) value)
           ((eq? request 'set-value!) set-my-value)
           ((eq? request 'forget) forget-my-value)
           ((eq? request 'connect) connect)
           (else (error "Unknown operation: CONNECTOR"
                        request))))
    me))


(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))



;检测器
(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  ;新设置值时打印该值
  (define (process-new-value)
    (print-probe (get-value connector)))
  ;撤销值时将对应的被撤销连接值打印为?
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  ;监测器类似于约束,安装在连接上,与连接相连,连接的值一旦发生变化,,唤醒除设置方之外的其他约束,此时检测器根据对应的操作进行下一步
  (connect connector me)
  me)

;乘积约束
(define (multiplier m1 m2 product)
  ;求值过程,在乘积约束中满足足够条件,对其他值连接进行求职,将该约束自身作为设置方
  (define (process-new-value)
    ;这里稍作修改，false无法应用于=
    (cond ((or (and (has-value? m1) (equal? 0 (get-value m2)))
               (and (has-value? m2) (equal? 0 (get-value m1))))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  ;对乘积约束的连接值进行撤销的过程——撤销操作只会在撤销方和该连接的当前值设置方相同时才会进行
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond((eq? request 'I-have-a-value) (process-new-value))
         ((eq? request 'I-lost-my-value) (process-forget-value))
         (else (error "Unknown request: MULTIPLIER"
                      request))))
  ;将该约束与三个连接相连
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

;常量约束
(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

;求和约束
(define (adder a1 a2 sum)
  ;求值过程,在求和约束中满足足够条件,对其他值连接进行求职,将该约束自身作为设置方
  (define (process-new-value)
    (cond((and (has-value? a1) (has-value? a1))
          (set-value! sum
                      (+ (get-value a1) (get-value a2))
                      me))
         ((and (has-value? a1) (has-value? sum))
          (set-value! a2
                      (- (get-value sum) (get-value a1))
                      me))
         ((and (has-value? sum) (has-value? a2))
          (set-value! a1
                      (- (get-value sum) (get-value a2))
                      me))))
  ; 对求和约束的连接值进行撤销的过程——撤销操作只会在撤销方和该连接的当前值设置方相同时才会进行
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  
  (define (me request)
    (cond((eq? request 'I-have-a-value) (process-new-value))
         ((eq? request 'I-lost-my-value) (process-forget-value))
         (else (error "Unknown request: ADDER" request))))
  ;将该约束与三个连接相连
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)



(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))


(define C (make-connector))
(define F (make-connector))
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(celsius-fahrenheit-converter C F)


(set-value! C 25 'user)










