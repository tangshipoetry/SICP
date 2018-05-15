#lang racket
; these have to be added to the polynomial package. So now it dispatches to 
; the generic procedures. 
  
(define (the-empty-termlist term-list) 
  (let ((proc (get 'the-empty-termlist (type-tag term-list)))) 
    (if proc 
        (proc) 
        (error "No proc found -- THE-EMPTY-TERMLIST" term-list)))) 
(define (rest-terms term-list) 
  (let ((proc (get 'rest-terms (type-tag term-list)))) 
    (if proc 
        (proc term-list) 
        (error "-- REST-TERMS" term-list)))) 
(define (empty-termlist? term-list) 
  (let ((proc (get 'empty-termlist? (type-tag term-list)))) 
    (if proc 
        (proc term-list) 
        (error "-- EMPTY-TERMLIST?" term-list)))) 
(define (make-term order coeff) (list order coeff)) 
(define (order term) 
  (if (pair? term) 
      (car term) 
      (error "Term not pair -- ORDER" term))) 
(define (coeff term) 
  (if (pair? term) 
      (cadr term) 
      (error "Term not pair -- COEFF" term))) 
  
; here is the term-list package, which has the constructors and selectors for  
; the dense and sparse polynomials. 
  
; the generic first-term procedure.  
(define (first-term term-list) 
  (let ((proc (get 'first-term (type-tag term-list)))) 
    (if proc 
        (proc term-list) 
        (error "No first-term for this list -- FIRST-TERM" term-list)))) 
  
; the pakcage with the constructors, selectors, and other helper procedures 
; I had to implement. 
(define (install-polynomial-term-package) 
  (define (first-term-dense term-list) 
    (if (empty-termlist? term-list) 
        '() 
        (list 
         (- (length (cdr term-list)) 1) 
         (car (cdr term-list)))))   
  (define (first-term-sparse term-list) 
    (if (empty-termlist? term-list) 
        '() 
        (cadr term-list))) 
  (define (prep-term-dense term) 
    (if (null? term) 
        '() 
        (cdr term)))                    ;-> only the coeff for a dense term-list 
  (define (prep-term-sparse term) 
    (if (null? term) 
        '() 
        (list term)))                   ;-> (order coeff) for a sparse term-list 
  (define (the-empty-termlist-dense) '(dense)) 
  (define (the-empty-termlist-sparse) '(sparse)) 
  (define (rest-terms term-list) (cons (type-tag term-list) (cddr term-list))) 
  (define (empty-termlist? term-list)  
    (if (pair? term-list)  
        (>= 1 (length term-list)) 
        (error "Term-list not pair -- EMPTY-TERMLIST?" term-list))) 
  (define (make-polynomial-dense var terms) 
    (make-polynomial var (cons 'dense (map cadr terms)))) 
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
  
(install-polynomial-term-package) 
  
  
; I had to changhe the adjoin-term procedure. It now does  
; zero padding so we can `mul` dense polynomials correctly.  
  
(define (zero-pad x type) 
  (if (eq? type 'sparse) 
      '() 
      (if (= x 0) 
          '() 
          (cons 0 (add-zeros (- x 1)))))) 
  
(define (adjoin-term term term-list) 
  (let ((preped-term ((get 'prep-term (type-tag term-list)) term)) 
        (preped-first-term ((get 'prep-term (type-tag term-list)) 
                            (first-term term-list)))) 
    (cond ((=zero? (coeff term)) term-list)  
          ((empty-termlist? term-list) (append (the-empty-termlist term-list)  
                                               preped-term 
                                               (zero-pad (order term) 
                                                         (type-tag 
                                                          term-list)))) 
          ((> (order term) (order (first-term term-list))) 
           (append (list (car term-list)) 
                   preped-term  
                   (zero-pad (- (- (order term) 
                                   (order (first-term term-list))) 
                                1) (type-tag term-list)) 
                   (cdr term-list))) 
          (else 
           (append preped-first-term  
                   (adjoin-term term (rest-terms term-list))))))) 
  
; here is `negate` now it creates a polynomial of the correct 
; type 
  
(define (negate p) 
  (let ((neg-p ((get 'make-polynomial (type-tag (term-list p))) 
                (variable p) (list (make-term 0 -1))))) 
    (mul-poly (cdr neg-p) p)))