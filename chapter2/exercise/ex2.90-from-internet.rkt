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









; these have to be added to the polynomial package. So now it dispatches to 
; the generic procedures. 

;空项相关——构造和判断
(define (the-empty-termlist term-list)
  (let([proc (get 'the-empty-termlist (type-tag term-list))])
    (if proc
        (proc)
        (error "No proc found -- THE-EMPTY-TERMLIST" term-list))))
(define (empty-termlist? term-list)
  (let([proc (get 'empty-termlist? (type-tag term-list))])
    (if proc
        (proc term-list)
        (error "-- EMPTY-TERMLIST?" term-list))))

;多项式中的单个项——次幂和系数
(define (make-term order coeff)
  (list order coeff))
(define (order term) 
  (if (pair? term) 
      (car term) 
      (error "Term not pair -- ORDER" term))) 
(define (coeff term) 
  (if (pair? term) 
      (cadr term) 
      (error "Term not pair -- COEFF" term)))

;获取项表中除第一项之外的其他项
(define (rest-terms term-list)
  (let([proc (get 'rest-terms (type-tag term-list))])
    (if proc
        (proc term-list)
        (error "-- REST-TERMS" term-list))))

;获取项表中第一项
(define (first-term term-list)
  (let([proc (get 'first-term (type-tag term-list))])
    (if proc
        (proc term-list)
        (error "No first-term for this list -- FIRST-TERM" term-list))))

;新项插入辅助过程
(define (add-zeros x)
  (if(= 0 x)
     '()
     (cons 0 (add-zeros (- x 1)))))

(define (zero-pad x type)
  (if(eq? type 'sparse)
     '()
     (if(= x 0)
        '()
        (add-zeros x))))
;在表中插入一个新的项
(define (adjoin-term term term-list)
  (let([preped-term ((get 'prep-term (type-tag term-list)) term)]
       [preped-first-term ((get 'prep-term (type-tag term-list))
                           (first-term term-list))])
    (cond((=zero? (coeff term)) term-list)
         ((empty-termlist? term-list)(append (the-empty-termlist term-list)
                                             preped-term
                                             (zero-pad (order term-list))))
         ((> (order term) (order (first-term term-list)))
          (append (list (car term-list))
                  preped-term
                  (zero-pad (- (- (order term)
                               (order (first-term term-list)))
                               1) 
                            (car term-list))
                  term-list))
         (else
          (append (list (car term-list));答案没有，我觉得应该有
                  preped-first-term
                  (adjoin-term term (rest-terms term-list)))))))

(define (install-polynomial-term-package)
  ;提取项表中第一项
  (define (first-term-dense term-list)
    (if(empty-termlist? term-list)
       '()
       (list
        (- (length (cdr term-list)) 1)
        (car (cdr term-list)))))
  (define (first-term-sparse term-list)
    (if(empty-termlist? term-list)
       '()
       (cadr term-list)))
  ;将要加到项表中的项进行合适处理
  (define (prep-term-dense term)
    (if(null? term)
       '()
       (cdr term)))
  (define (prep-term-sparse term)
    (if(null? term)
       '()
       (list term)))
  ;不同的空项表
  (define (the-empty-termlist-dense) '(dense)) 
  (define (the-empty-termlist-sparse) '(sparse))
  ;提取项表中除去第一个外其他的项表
  (define (rest-terms term-list)
    (if(<= (length term-list) 1)
       (error "No-rest-terms" term-list)
       (cons (type-tag term-list) (cddr term-list))))
  ;判断项表是否为空
  (define (empty-termlist? term-list)
    (if(pair? term-list)
       (>= 1 (length term-list))
       (error "Term-list not pair -- EMPTY-TERMLIST?" term-list)))
  ;构造多项式
  (define (make-polynomial-dense var terms)
    (make-polynomial var (cons 'dense (map cadr terms))));terms是将make-term输出用列表安排,所以有这里的map操作
  (define (make-polynomial-sparse var terms)
    (make-polynomial var (cons 'sparse terms)))
  (put 'first-term 'sparse  
       (lambda (term-list) (first-term-sparse term-list))) 
  (put 'first-term 'dense 
       (lambda (term-list) (first-term-dense term-list))) 
  (put 'prep-term 'dense 
       (lambda (term) (prep-term-dense term))) 
  (put 'prep-term 'sparse 
       (lambda (term) (prep-term-sparse term))) 
  (put 'rest-terms 'dense 
       (lambda (term-list) (rest-terms term-list))) 
  (put 'rest-terms 'sparse 
       (lambda (term-list) (rest-terms term-list))) 
  (put 'empty-termlist? 'dense 
       (lambda (term-list) (empty-termlist? term-list))) 
  (put 'empty-termlist? 'sparse 
       (lambda (term-list) (empty-termlist? term-list))) 
  (put 'the-empty-termlist 'dense 
       (lambda () (the-empty-termlist-dense))) 
  (put 'the-empty-termlist 'sparse 
       (lambda () (the-empty-termlist-sparse))) 
  (put 'make-polynomial 'sparse 
       (lambda (var terms) (make-polynomial-sparse var terms))) 
  (put 'make-polynomial 'dense 
       (lambda (var terms) (make-polynomial-dense var terms))) 
  'done)

 ;-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;多项式安装包
(define (install-polynomial-package)
  (define (add-terms L1 L2)
    (cond((empty-termlist? L1) L2)
         ((empty-termlist? L2) L1)
         (else
          (let([t1 (first-term L1)][t2 (first-term L2)])
            (cond((> (order t1) (order t2))
                  (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if(empty-termlist? L1)
       (the-empty-termlist)
       (add-terms (mul-term-by-all-terms (first-term L1) L2)
                  (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if(empty-termlist? L)
       (the-empty-termlist)
       (let((t2 (first-term L)))
         (adjoin-term
          (make-term (+ (order t1)(order t2))
                     (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (empty-termlist? terms)(null? terms))
  (define (the-empty-termlist) null)
  (define (first-term terms)(car terms))
  (define (rest-terms terms)(cdr terms))
  (define (adjoin-term term term-list)
    (if(=zero? (coeff term))
       (term-list)
       (cons term term-list)))
  (define (make-term order coeff)
    (list order coeff))
  (define (order term)(car term))
  (define (coeff term)(cadr term))
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (add-poly p1 p2)
    (if(same-variable? (variable p1) (variable p2))
       (make-poly (variable p1)
                  (add-terms (term-list p1)
                             (term-list p2)))
       (error "Polys not in same var -- ADD-POLY" (list p1 p2))))
  (define (sub-poly p1 p2)
    (add-poly p1 (get-negtive p2)))
  (define (mul-poly p1 p2)
    (if(same-variable? (variable p1) (variable p2))
       (make-poly (variable p2)
                  (mul-terms (term-list p1)
                             (term-list p2)))
       (error "Polys not in same var -- AMUL-POLY" (list p1 p2))))
  (define (tag p) (attach-tag 'polynomial p))
  (define (poly? x)
    (eq? 'polynomial (type-tag x)))

  (define (zero-terms? termlist) 
    (or (empty-termlist? termlist) 
        (and (=zero? (coeff (first-term termlist))) 
             (zero-terms? (rest-terms termlist)))))
  
  (define (zero-poly? term-coeff)
    (zero-terms? (term-list term-coeff)))
  (define (get-negtive-terms termlist)
    (if(empty-termlist? termlist)
       (the-empty-termlist)
       (let([t (first-term termlist)])
         (adjoin-term (make-term (order termlist)
                                 (get-negtive (coeff termlist)))
                      (get-negtive-terms (rest termlist))))))
  (define (get-negtive-poly x)
    (make-poly (variable x)
               (get-negtive-terms (term-list x))))

  (define (make-polynomial var terms)
    ((get 'make (type-tag terms)) var terms))
  
  (put 'add '(polynomial c)
       (lambda(p1 p2)(tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda(p1 p2)(tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda(var terms)(tag (make-poly var terms))))
  (put '=zero? 'polynomial
       zero-poly?)
  (put 'get-negtive '(polynomial)
       get-negtive-poly)
  (put 'make '(polynomial)
       (lambda(var terms)(tag (make-polynomial var terms))))
  
  'done)

(define (make-polynomial var terms)
  ((get 'make '(polynomial)) var terms))


;目前基本理解，但是，个人觉得有一点问题，个人觉得添加标记应该在对外接口的时候进行,此外应该将dense和sparse分两个包

;此外按照书本例子，个人觉得应该是分dense和sparse两个包实现，每个里面提供另一个的选择和构造过程——但是实际实现有问题，是转化还是其他操作暂不明了
















