#lang racket

(define (set-cdr! pair value)
  (cons (car pair)
        value))

(define (make-table)
  (let ([local-table (list '*table*)])
    (define (lookup key-1 key-2)
      (let([subtable (assoc key-1 (cdr local-table))])
        (if subtable
            (let ([record (assoc key-2 (cdr subtable))])
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ([subtable (assoc key-1 (cdr local-table))])
        (if subtable
            (let ([record (assoc key-2 (cdr subtable))])
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons (key-2 value)))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond((eq? m 'lookup-proc) lookup)
           ((eq? m 'insert-proc) insert!)
           (else (error "Unknow operation-----TABLE" m))))
    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))

;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------
(define (gcd a b)
  (if(= b 0)
     a
     (gcd b
          (remainder a
                     b))))

;求平方值
(define (square x)(* x x))
;求平方根
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
;改进方法
(define (improve guess x)
  (average (/ x guess) guess))
(define (average x y)(/ (+ x y) 2.0))
;判断当前值是否足够好
(define (good-enough? guess x)
 (< (abs (- (square guess) x)) 0.0000001))

;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------

(define (attach-tag data-tag contents)
  (if(number? contents)
     contents
     (cons data-tag contents)))

(define (type-tag datum)
  (if(number? datum)
     'scheme-number
     (if(pair? datum)
        (car datum)
        (error "bad tagged datum" datum))))
(define (contents datum)
  (if(number? datum)
     datum
     (if(pair? datum)
        (cdr datum)
        (error "bad tagged datum" datum))))

#|
(define (apply-generic op . args)
  (let((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))
|#



;直角坐标复数包
(define (install-rectangular-package)
  ;internal procedure
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-rreal-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z)(real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a))(* r (sin a))))
  
  ;interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda(x y)(tag (make-from-rreal-imag x y))))
  (put 'make-from-mag-ang 'rectangular
    (lambda(r a)(tag (make-from-mag-ang r a))))
  'done)

;极坐标复数包
(define (install-polar-package)
  
  (define (manitude z) (cdr z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (manitude z) (cos (angle z))))
  (define (imag-part z) (* (manitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda(x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda(r a)(tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define (add x y)(apply-generic 'add x y))
(define (sub x y)(apply-generic 'sub x y))
(define (mul x y)(apply-generic 'mul x y))
(define (div x y)(apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda(x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda(x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda(x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda(x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda(x) (tag x)))
  (put equ? '(scheme-number scheme-number)
       (lambda(x y)(= x y)))
  (put zero? 'scheme-number
       (lambda(x)(= x 0)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ([g (gcd n d)])
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational)
       (lambda(x y)(tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda(x y)
         (= (* (numer x) (denom y))
            (* (numer y) (denom x)))))
  (put zero? 'rational
       (lambda(x)(= 0 (numer x))))
  'done)

(define (make-ration n d)
  ((get 'make 'rational) n d))


(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'equ? '(complex complex)
       (lambda(x y)
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y)))))
  (put 'zero 'complex
       (lambda(x)
         (and (and (= 0 (real-part x))
                   (= 0 (imag-part x))))))
  
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (equ? x y)
  (apply-generic equ? x y))

(define (zero? x)
  (apply-generic zero? x))



#|
(define (apply-generic op . args)
  (let([type-tags (map type-tag args)])
    (let([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (if(= (length args) 2)
             (let([type1 (car type-tags)]
                  [type2 (cadr type-tags)])
               (if(equal?type1 type2)
                  (error "No methods for these types" (list op type-tags))
                  (let([t1->t2 (get-coercion type1 type2)]
                       [t2->t1 (get-coercion type2 type1)]
                       [a1 (car args)]
                       [a2 (cadr args)])
                    (cond(t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                         (t2->t1
                          (apply-generic op a1 (t2->t1 a2)))
                         (else
                          (error "No methods for these types" (list op type-tags)))))))
             (error "No methods for these types" (list op type-tags)))))))
|#

(define (indenity x) x)

(define (apply-generic op . args)

  ;递归
  ;根据已有标记对标记列表进行迭代查找强转过程,找到则将其放置于结果集构建的列表中,
  ;迭代过程中如果出现无法强转的情况则返回false或null,(这里两种情况，为false和null影响到下一步判断,这里采用null,下一步比较长度确定是否能全强转)
  ;如果一直到最后都能进行强转，则返回所有强转过程结果集
  (define (iter tags tag result)
    (if(null? tags)
       result
       (let([cur-tag (car tags)])
         (if(eq? cur-tag tag)
            (cons indenity (iter (cdr tags) result))
            (let([t1->t2 (get-coercion cur-tag tag)])
              (if t1->t2
                  (cons t1->t2 (iter (cdr tags result)))
                  null))))))   

  #|
;迭代,这里的结果需要反转
  (define (iter tags tag result)
    (if(null? tags)
       result
       (let([cur-tag (car tags)])
         (if(eq? cur-tag tag)
            (iter (cdr tags) (cons identity result))
            (let([t1->t2 (get-coercion cur-tag tag)])
              (if t1->t2
                  (iter (cdr tags) (cons t1->t2 result))
                  null))))))


思路：直接构造和参数列表长度一样的某一参数标记的列表 执行(apply-generic op (list tags...))
|#


  
  
  #|
依次将参数表中所有参数依次进行强转,若其他所有参数转化当前参数类型,则返回强转过程列表(遇到相同类型已在过程列表中放入相应过程)
若参数表中其他参数无法全部转化为当前参数类型，则对下一个参数进行该操作，
若参数表为空则证明参数列表中所有参数无法统一强转为已有类型，这种情况下表明没有相应过程返回初始结果（这里为null）
|#
  (define args-type-tags (map type-tag args))
  (define (iter-args args)
     (if(null? args)
         null
         (let ([current-tag (type args)])
           (let ([result (iter args-type-tags current-tag null)])
             (if(= (length result) (length args-type-tags))
                result
                (iter-args (cdr args)))))))
  
  (let([tape-tags (map type-tag args)])
    (let([proc (get op type-tags)])
      (if proc
          (apply proc (map contents args))
          (if(>= (length args) 2)
             (let([result (iter-args args)])
               (if(null? result)
                  (error "No methods for these types" (list op type-tags))
                 (apply-generic op (map result args))))
             (error "No methods for these types" (list op type-tags)))))))



























