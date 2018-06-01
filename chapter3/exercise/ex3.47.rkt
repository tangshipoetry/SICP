#lang sicp


(define (make-serializer)
  (let([mutex (make-mutex)])
    (lambda(p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let([val (apply p args)])
          (mutex 'release)
          val))
      serialized-p)))

;这里还需要原子性----目前还不具备
(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))




;基本实现---不安全
(define (make-semaphore n)

  (define (acquire)
    (if (> n 0)
        (begin (set! n (- n 1))
               'ok)
        (acquire)))

  (define (release)
    (set! n (+ n 1))
    'ok)

  (define (dispatch mode)
    (cond ((eq? mode 'acquire)
           (acquire))
          ((eq? mode 'release)
           (release))
          (else
           (error "Unknown mode MAKE-SEMAPHORE" mode))))

  dispatch)




;互斥元实现
(define (make-semaphore n)

  (let ((mutex (make-mutex)))
        
    (define (acquire)
      (mutex 'acquire)
      (if (> n 0)                     ; 用互斥元保护 n 的修改
          (begin (set! n (- n 1))     ; 获取信号量之后
                 (mutex 'release)     ; 释放互斥元
                 'ok)
          (begin (mutex 'release)     ; 获取信号量不成功,先释放互斥元
                 (acquire))))         ; 然后再次尝试获取信号量

    (define (release)
      (mutex 'acquire)
      (set! n (+ n 1))                ; release 操作也需要用互斥元保护
      (mutex 'release)
      'ok)

    (define (dispatch mode)
      (cond ((eq? mode 'acquire)
             (acquire))
            ((eq? mode 'release)
             (release))
            (else
             (error "Unknown mode MAKE-SEMAPHORE" mode))))

    dispatch))



(define (make-semaphore n)

    (define (acquire)
        (if (test-and-set! n)
            (acquire)
            'ok))
    
    (define (release)
        (set! n (+ n 1))
        'ok)

    (define (dispatch mode)
        (cond ((eq? mode 'acquire)
                (acquire))
              ((eq? mode 'release)
                (release))
              (else
                (error "Unknown mode MAKE-SEMAPHORE" mode))))

    dispatch)

(define (test-and-set! n)
    (if (= n 0)
        #t
        (begin (set! n (- n 1))
               #f)))






























