#lang racket

#|
;网上的，能用，看不太懂
(define (1+ x)(+ 1 x))

(define (nearby? j k)
   (>= 1 (abs (- j k))))
  
(define (ordinary-multiple-dwelling)
  (define (display-solution f s c m b)
    (display
     (list (list 'baker b) (list 'cooper c)
           (list 'fletcher f) (list 'miller m)
           (list 'smith s)))
    (newline))
  (define (iter-f f)
    (cond ((= f 1) (iter-f 2))
          ((= f 5) 'done)
          (else (iter-s f 1))))
  (define (iter-s f s)
    (cond ((> s 5) (iter-f (1+ f)))
          ((nearby? f s) (iter-s f (1+ s)))  ;; see additional note 
          (else (iter-c f s 1))))
  (define (iter-c f s c)
    (cond ((> c 5) (iter-s f (1+ s)))
          ((or (nearby? f c) (= c 1) (= c s))
           (iter-c f s (1+ c)))
          (else (iter-m f s c 1))))
  (define (iter-m f s c m)
    (cond ((> m 5) (iter-c f s (1+ c)))
          ((or (<= m c) (= m s) (= m f))
           (iter-m f s c (1+ m)))
          (else (iter-b f s c m 1))))
  (define (iter-b f s c m b)
    (cond ((> b 5) (iter-m f s c (1+ m)))
          (else (if (not (or (= b 5) (= b m) (= b c) (= b s) (= b f)))
                    (display-solution f s c m b))
                (iter-b f s c m (1+ b)))))
  (iter-f 1))
  
(ordinary-multiple-dwelling)

|#



;网上的，大致懂了
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))


(define (flatmap proc li)
  (if (null? li)
      '()
      (let ((result (proc (car li)))
            (rest (flatmap proc (cdr li))))
        (if (pair? result)
            (append result rest)
            (cons result rest)))))
  
(define (permutations lists)
  (if (null? lists)
      '(())
      (flatmap (lambda (x)
                 (map (lambda (y) (cons x y))
                      (permutations (cdr lists))))
               (car lists))))
  
(define (restrictions l)
  (apply
   (lambda (baker cooper fletcher miller smith)
     (and (> miller cooper)
          (not (= (abs (- smith fletcher)) 1))
          (not (= (abs (- fletcher cooper)) 1))
          (distinct? (list baker cooper fletcher miller smith))))
   l))

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (mutiple-dwelling)
  (let ((baker '(1 2 3 4))
        (cooper '(2 3 4 5))
        (fletcher '(2 3 4))
        (miller '(3 4 5))
        (smith '(1 2 3 4 5)))
    (filter restrictions (permutations (list baker cooper fletcher miller smith)))))




























