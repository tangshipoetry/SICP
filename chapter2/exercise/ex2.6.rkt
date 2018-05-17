#lang racket

(define zero
  (lambda(f)
    (lambda(x) x)))

(define (add-1 n)
  (lambda(f)
    (lambda(x)
      (f ((n f) x)))))

#|
(add-1 zero)

((lambda(f)
    (lambda(x)
      (f ((n f) x))))
 (lambda(f)
    (lambda(x) x))
)

(lambda(f)
    (lambda(x)
      (f ((lambda(x) x) x)))

(lambda(f)
    (lambda(x)
      (f x))



|#
(define one (add-1 zero))

(define two (add-1 one))

(define plus
  (lambda(first second)
    (lambda(f)
      (lambda(x)
        ((first f)((second f) x))))))


(define (f x)
  (display "*")
  )






(define (start-test-2-6)
  
  (display "going to display 1:")(newline)
  ((one f) 'a)(newline)
  (display "going to display 2:")(newline)
  ((two f) 'a) (newline)
  (display "going to display 1+2:")(newline)
  (((plus one two) f) 'a)
  (newline)
  (display "going to display 1+2+2")(newline)
  (((plus (plus one two) two) f) 'a)
  (newline)

  (display "end.") (newline))
























