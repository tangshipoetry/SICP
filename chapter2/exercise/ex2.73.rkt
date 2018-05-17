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

#|

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
        (else
         (error "unknown expression type: DERIV" exp))))
|#

(define (operator exp)(car exp))
(define (operand exp)(car exp))
;a)  number?和same-variable?无法安装到表格中，因为没有所谓operand和operator
(define (deriv exp var)
  (cond((number? exp) 0)
       ((variable? exp)
        (if(same-variable? exp var) 1 0))
       (else ((get 'deriv (operator exp)) (operands exp)
                                          var))))

(defile (install-addition-deriv)
  (define (add-deriv operands var)
    (apply + (map (lambda(x)
                    (deriv x var))
                  operands)))
  (put 'deriv '+ add-deriv)
  'done)


(define (install-multiply-deriv)
  (define (mul-deriv operands var)
    (let([m1 (multiplier operands)][m2 (multiplicand operands)])
      (make-sum
       (make-product
        m2
        (deriv m1))
       (make-product
        m1
        (deriv m2)))))
  (put 'deriv * mul-deriv)
  'done)






#|
网上摘抄
(define (install-sum-package)

    ;;; internal procedures 
    (define (addend s)
        (car s))

    (define (augend s)
        (cadr s))

    (define (make-sum x y)
        (cond ((=number? x 0)
                y)
              ((=number? y 0)
                x)
              ((and (number? x) (number? y))
                (+ x y))
              (else
                (attach-tag '+ x y))))

    ;;; interface to the rest of the system
    (put 'addend '+ addend)
    (put 'augend '+ augend)
    (put 'make-sum '+ make-sum)

    (put 'deriv '+
        (lambda (exp var)
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var))))

'done)

(define (make-sum x y)
    ((get 'make-sum '+) x y))

(define (addend sum)
    ((get 'addend '+) (contents sum)))

(define (augend sum)
    ((get 'augend '+) (contents sum)))



(define (install-product-package)

    ;;; internal procedures
    (define (multiplier p)
        (car p))

    (define (multiplicand p)
        (cadr p))

    (define (make-product x y)
        (cond ((or (=number? x 0) (=number? y 0))
                0)
              ((=number? x 1)
                y)
              ((=number? y 1)
                x)
              ((and (number? x) (number? y))
                (* x y))
              (else
                (attach-tag '* x y))))

    ;;; interface to the rest of the system
    (put 'multiplier '* multiplier)
    (put 'multiplicand '* multiplicand)
    (put 'make-product '* make-product)

    (put 'deriv '*
        (lambda (exp var)
            (make-sum
                (make-product (multiplier exp)
                              (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var)
                              (multiplicand exp)))))

'done)

(define (make-product x y)
    ((get 'make-product '*) x y))

(define (multiplier product)
    ((get 'multiplier '*) (contents product)))

(define (multiplicand product)
    ((get 'multiplicand '*) (contents product)))



|#




























