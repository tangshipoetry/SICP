#lang racket

(define (=number? a b)
  (and (number? a)
       (= a b)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (** base e)
  (expt base e))

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))
(define (base x)
  (cadr x))
(define (exponent x)
  (caddr x))



(define (make-sum a1 a2)
  (cond((=number? a1 0) a2)
       ((=number? a2 0) a1)
       ((and (number? a1)
             (number? a2))
        (+ a1 a2))
       (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond((=number? m1 0) 0)
       ((=number? m2 0) 0)
       ((and (number? m1)
             (number? m2))
        (* m1 m2))
       (else (list '* m1 m2))))

(define (make-exponentiation base e)
  (cond((=number? e 0) 1)
       ((=number? e 1) base)
       ((and (number? base)
             (number? e))
        (** base e))
       (else (list '** base e))))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)));(make-exponentiation (base exp) (list '- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))



































































































