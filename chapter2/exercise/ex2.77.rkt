#lang racket

;原操作
(magnitude z)

(apply-generic 'magnitude z)

(map type-tag (list z))         ; => '(complex)

(get 'magnitude '(complex))     ; => #f

(error ...) 


;添加后
(magnitude z)                                                   ; 这个 magnitude 是最外层的通用操作

(apply-generic 'magnitude z)

(map type-tag (list z))                                         ; => '(complex)

(get 'magnitude '(complex))                                     ; => magnitude  ; 这个 magnitude 是定义于 complex 包中的 magnitude

(apply magnitude (map contents (list z)))                       ; =>  (apply magnitude '((rectangular 3 . 4)))

(magnitude '(rectangular 3 . 4))

(apply-generic 'magnitude '(rectangular 3 . 4))

(map type-tag (list '(rectangular 3 . 4)))                      ; => '(rectangular)

(get 'magnitude '(rectangular))                                 ; => magnitude  ; 这个 magnitude 是定义于 rectangular 包中的 magnitude

(apply magnitude (map contents (list '(rectangular 3 . 4))))    ; => (apply magnitude '((3 . 4)))

(magnitude '(3 . 4))

(sqrt (+ (square (real-part '(3 . 4)))
         (square (imag-part '(3 . 4)))))

5

















;网上摘抄
(define (install-complex-package)

    ;;; imported procedures from rectangular and polar packages
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))

    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang 'polar) r a))

    ;;; interal procedures
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

    ;;; interface to rest of the system
    (define (tag z)
        (attach-tag 'complex z))

    (put 'add '(complex complex)
        (lambda (z1 z2)
            (tag (add-complex z1 z2))))

    (put 'sub '(complex complex)
        (lambda (z1 z2)
            (tag (sub-complex z1 z2))))

    (put 'mul '(complex complex)
        (lambda (z1 z2)
            (tag (mul-complex z1 z2))))

    (put 'div '(complex complex)
        (lambda (z1 z2)
            (tag (div-complex z1 z2))))

    (put 'make-from-real-imag 'complex
        (lambda (x y)
            (tag (make-from-real-imag x y))))

    (put 'make-from-mag-ang 'complex
        (lambda (r a)
            (tag (make-from-mag-ang r a))))

    ; 新增
    (put 'real-part '(complex) real-part)

    (put 'imag-part '(complex) imag-part)

    (put 'magnitude '(complex) magnitude)

    (put 'angle '(complex) angle)

'done)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))


























