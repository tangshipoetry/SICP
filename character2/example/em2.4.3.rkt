#lang racket

(define complex-paclage null)
(define (put op type item)
  (cons (list op type item) complex-paclage))
(define (get op type)
  (if(null? complex-paclage)
     #f
     (let([current (car complex-paclage)])
       (if(and (equal? op (car current))
               (equal? type (cadr current)))
          (caddr current)
          (let ([complex-paclage (cdr complex-paclage)])
            (get op type))))))

(define (square x)(expt x 2))
(define (attach-tag data-tag contents) (cons data-tag contents))

(define (type-tag datum)
  (if(pair? datum)
     (car datum)
     (error "bad tagged datum" datum)))
(define (contents datum)
  (if(pair? datum)
     (cdr datum)
     (error "bad tagged datum" datum)))


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
  (put 'magnitude '(rectangular) manitude)
  (put 'angle (rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda(x y)(tag (make-from-rreal-imag x y))))
  (define 'make-from-mag-ang 'rectangular
    (lambda(r a)(tag (make-from-mag-ang r a))))
  'done)


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



(define (apply-generic op . args)
  (let((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))


(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))



(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))


(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

















