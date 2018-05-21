#lang racket


#|
(define (loop? lst)
  (define (iter x y)
    (let ((x-walk (list-walk 1 x))
          (y-walk (list-walk 2 y)))
      (cond ((or (null? x-walk) (null? y-walk))
             #f)
            ((eq? x-walk y-walk)
             #t)
            (else
             (iter x-walk y-walk)))))
  (iter lst lst))

(define (list-walk step lst)
  (cond ((null? lst)
         '())
        ((= step 0)
         lst)
        (else
         (list-walk (- step 1)
                    (cdr lst)))))
|#





 (define (contains-cycle? lst) 
   (define (safe-cdr l) 
     (if (pair? l) 
         (cdr l) 
         '())) 
   (define (iter a b) 
     (cond ((not (pair? a)) #f) 
           ((not (pair? b)) #f) 
           ((eq? a b) #t) 
           ((eq? a (safe-cdr b)) #t) 
           (else (iter (safe-cdr a) (safe-cdr (safe-cdr b)))))) 
   (iter (safe-cdr lst) (safe-cdr (safe-cdr lst))))































