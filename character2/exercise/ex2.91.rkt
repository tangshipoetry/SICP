#lang racket

;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------
;put和get相关
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
;数据标记相关
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
;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------
;类型升降
(define (project x)
  (let([proc (get project (type-tag x))])
    (if proc
        (proc x)
        #f)))

(define (drop x)
  (let([down (project x)])
    (if down
        (let([up (raise x)])
          (if(equ? x up)
             (drop down)
             x))
        x)))
;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------;---------
;通用操作依赖
(define (apply-generic op . args)
  ;如果存在,将前一个数迭代提升到后一个数的类型,过程中不存在不存在返回false
  (define (iter-rise a b)
    (define (same-type? a b)
      (let([tpye-a (type-tag a)][type-b (type-tag b)])
        (eq? a b)))
    (let([proc (raise a)])
      (if proc
          (let([raised-a (proc a)])
            (if(same-type? raised-a b)
               raised-a
               (iter-rise raised-a b)))
          #f)))
  (let([type-tags (map type-tag args)])
    (let([proc (get op type-tags)])
      (if proc
          (drop (apply proc (map contents args))) 
          ;(apply proc (map contents args))
          (if(= (length args) 2)
             (let([a1 (car args)][a2 (cadr args)])
               (let([raised-a1 (iter-rise a1 a2)][raised-a2 (a2 a1)])
                 (cond (raised-a1 (apply-generic op raised-a1 a2))
                       (raised-a2 (apply-generic op a1 raised-a2))
                       (else (error "No methods for these types" (list op type-tags))))))
             (error "No methods for these types" (list op type-tags)))))))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;安装整数包
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
  (put 'equ? '(scheme-number scheme-number)
       (lambda(x y)(= x y)))
  (put '=zero? 'scheme-number
       (lambda(x)(= x 0)))
  (put 'raise 'scheme-number
       (lambda(x)(make-ration x 1)))
  (put 'sine 'scheme-number
       (lambda(x)(tag (sin x))))
  (put 'cosine 'scheme-number
       (lambda(x)(tag (cos x))))
  (put 'arctan '(scheme-number)
       (lambda (x) (tag (atan x))))
  (put 'ex '(scheme-number)
       (lambda(x e)(tag (exp x e))))
  (put 'get-negtive 'scheme-number
       (lambda(x)(tag (- x))))
  'done)
#|
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
|#
;------------------------------------------------------------------------------------------------------------------------------------------------
;安装有理数包
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
  (put '=zero? 'rational
       (lambda(x)(= 0 (numer x))))
  (put 'raise 'rational
       (lambda(x)(make-complex-from-real-imag x 0)))
  (put 'project 'rational
       (lambda(x)(make-scheme-number (quotient (numer x) (denom x)))))
  (put 'sine 'rational
       (lambda(x)(tag (sin (/ (numer x) (denom x))))))
  (put 'cosine 'rational
       (lambda(x)(tag (cos (/ (numer x) (denom x))))))
  (put 'arctan '(rational)
       (lambda (x) (tag (atan (/ (numer x) (denom x))))))
  (put 'ex '(rational)
       (lambda(x e)(tag (exp (/ (numer x) (denom x)) (/ (numer e) (denom e))))))
  (put 'get-negtive 'rational
       (lambda(rat)(make-ration (get-negtive (numer rat)) (denom rat))))
  'done)
#|
(define (make-ration n d)
  ((get 'make 'rational) n d))
|#
;------------------------------------------------------------------------------------------------------------------------------------------------
;直角坐标复数包
(define (install-rectangular-package)
  ;internal procedure
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-rreal-imag x y) (cons x y))
  (define (magnitude z)
    (ex (add (ex (real-part z) 2)
             (ex (imag-part z) 2))
          0.5))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a))(mul r (sine a))));原:(cons (* r (cos a))(* r (sin a))))
  
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
  (define (real-part z) (mul (manitude z) (cosine (angle z))))
  (define (imag-part z) (mul (manitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (ex (add (ex x 2) (ex y 2)) 0.5)
          (arctan y x)))
  
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

;------------------------------------------------------------------------------------------------------------------------------------------------

;安装复数包
(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
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
;------------------------------------------------------------------------------------------------------------------------------------------------
;存疑,此处可能需要重构有理数,由于此处复数和有理数之间没有实数,所以或许应该求出其中的numer和denom用make-ration重构有理数,但这样做默认本身是有理数,而实际有可能是其他类型,甚至复数
  (put 'project 'complex
       (lambda(x)(real-part x)))

  (put 'get-negtive 'complex
       (lambda(x)(make-complex-from-real-imag (get-negtive real-part)
                                              (get-negtive imag-part))))

  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (make-ration n d)
  ((get 'make 'rational) n d))
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
;----------------------------------------------------------------------------------------------------------------------------------------------------------------
;封装apply-generic的通用操作
(define (add x y)(apply-generic 'add x y))
(define (sub x y)(apply-generic 'sub x y))
(define (mul x y)(apply-generic 'mul x y))
(define (div x y)(apply-generic 'div x y))

(define (equ? x y)
  (apply-generic equ? x y))

(define (=zero? x)
  (apply-generic =zero? x))

(define (raise x)
  (apply-generic 'raise x))


(define (sine x) (apply-generic 'sine x)) 
(define (cosine x) (apply-generic 'cosine x)) 
(define (ex x e)(apply-generic 'ex x e))
(define (arctan x) (apply-generic 'arctan x))
(define (get-negtive x)(apply-generic 'get-negtive x))

;----------------------------------------------------------------------------------------------------------------------------------------------------------------
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;这里分开实现,网上答案看不明白





























