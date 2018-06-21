#lang racket

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
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


(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (if(check? first)
                   (let ([val (cond-predicate first)]
                         [proc (caddr first)]);
                     (make-if (cond-predicate first)
                              ((caddr first) (cond-predicate first))
                              (expand-clauses rest)))
                   (sequence->exp (cond-actions first)))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (if(check? first)
               (let ([val (cond-predicate first)]
                     [proc (caddr first)]);
                 (make-if (cond-predicate first)
                          ((caddr first) (cond-predicate first))
                          (expand-clauses rest)))
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))

(define (check? clause)
  (eq? => (cadr clause)))







(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))























