#lang racket

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((let? exp) (eval (let->combination exp) env))
        
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp)(and-squ-eval (and-content exp) env))
        ((or? exp) (or-squ-eval (or-content exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))



(for (variable start end)
  body)

(define (for? exp)
  (tagged-list exp 'for))
(define (for-variable exp)
  (caadr exp))
(define (for-start exp)
  (cadadr exp))
(define (for-end exp)
  (caddadr exp))
(define (for-body exp)
  (cddr exp))

(define (expand-range start end proc)
  (cond
    ((< start end)
     (proc start)
     (expand-range (+ start 1) end proc))))

(define (for->combination exp)
  (expand-range (for-start)
                (for-end)
                (make-lambda (list var)
                             (for-body exp))))




(while (predicate variable)
       body)

(define (while? exp)
  (tagged-list exp 'while))

(define (while-predicate exp)
  (caadr exp))

(define (while-variable exp)
  (cadadr exp))

(define (while-body exp)
  (cddr exp))

(define (expand-predicate predicate i body)
  (if (predicate i)
      (cons 'begin body)))

(define (while->combination exp)
  (expand-predicate
   (while-predicate exp)
   (while-variable exp)
   (while-body exp)))

























































