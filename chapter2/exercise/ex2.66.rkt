#lang racket


#|
(define (lookup given-key set-of-records)
  (cond((null? set-of-records) #f)
       ((equal? given-key (key (car set-of-records))) #t)
       (else (lookup given-key (cdr set-of-records)))))
|#


(define (lookup given-key set-of-records)
  (if(null? set-of-records)
     #f
     (let ([current (car set-of-records)] [left (left-branch set-of-records)] [right (right-branch set-of-records)])
       (cond((= given-key (key current)) #t)
            ((> given-key (key current)) (lookup given-key right))
            ((< given-key (key current)) (lookup given-key left))))))


























