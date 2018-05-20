#lang racket


(define (make-account1 balance)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds"))
    (define (deposit amount)
        (set! balance (+ balance amount)))
    (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw) 
              ((eq? m 'deposit) deposit)
              (else
                (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch)



(define make-account

  (lambda (balance)

    (define withdraw
      (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds")))

    (define deposit 
      (lambda (amount)
        (set! balance (+ balance amount))))

    (define dispatch
      (lambda (m)
        (cond ((eq? m 'withdraw)
               withdraw)
              ((eq? m 'deposit)
               deposit)
              (else
               (error "Unknown request -- MAKE-ACCOUNT" m)))))

    dispatch))


;--------------------------------------------------------------------------------------------------------------------------------------------------------

         +-------------------------------------+
global -> |                                     |
env       | make-account                        |
          +----|--------------------------------+
               |       ^
               |       |
               v       |
             [*][*]----+
              |
              |
              v
  parameters: balance
  body: (define withdraw ...)
        (define deposit ...)
        (define dispatch ...)
        dispatch



          +----------------------------------------------------+
global -> |                                                    |
env       | make-account                                       |
          +----|-----------------------------------------------+
               |       ^                     ^
               |       |                     |
               v       |           E1 -> +------------------+
             [*][*]----+                 | balance: 50      |<----------+
              |                          |                  |           |
              |                          | withdraw --------------->[*][*]----> parameters: amount
              v                          |                  |                   body: ...
  parameters: balance                    |                  |<----------+
  body: (define withdraw ...)            |                  |           |
        (define deposit ...)             | deposit ---------------->[*][*]----> parameters: amount
        (define dispatch ...)            |                  |                   body: ...
        (lambda (m) ...)                 |                  |<----------+
                                         |                  |           |
                                         | dispatch --------------->[*][*]----> parameters: m
                                         |                  |                   body: ...
                                         +------------------+
                                         dispatch



          +----------------------------------------------------+
global -> |                                                    |
env       | make-account      acc                              |
          +----|---------------|-------------------------------+
               |       ^       |             ^
               |       |       |             |
               v       |       |   E1 -> +------------------+
             [*][*]----+       |         | balance: 50      |<----------+
              |                |         |                  |           |
              |                |         | withdraw --------------->[*][*]----> parameters: amount
              v                |         |                  |                   body: ...
  parameters: balance          |         |                  |<----------+
  body: (define withdraw ...)  |         |                  |           |
        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
        (define dispatch ...)  |         |                  |                   body: ...
        (lambda (m) ...)       |         |                  |<----------+
                               |         |                  |           |
                               +---------->dispatch --------------->[*][*]----> parameters: m
                                         |                  |                   body: ...
                                         +------------------+


          +----------------------------------------------------+
global -> |                                                    |
env       | make-account      acc                              |
          +----|---------------|-------------------------------+
               |       ^       |             ^
               |       |       |             |
               v       |       |   E1 -> +------------------+
             [*][*]----+       |         | balance: 50      |<----------+
              |                |         |                  |           |
              |                |         | withdraw --------------->[*][*]----> parameters: amount
              v                |         |                  |                   body: ...
  parameters: balance          |         |                  |<----------+
  body: (define withdraw ...)  |         |                  |           |
        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
        (define dispatch ...)  |         |                  |                   body: ...
        (lambda (m) ...)       |         |                  |<----------+
                               |         |                  |           |
                               +---------->dispatch --------------->[*][*]----> parameters: m
                                         |                  |                   body: ...
                                         +------------------+
                                                    ^
                                                    |
                                                    |
                                   (acc 'deposit)   |
                                                    |
                                            +-------------+
                                            |             |
                                      E2 -> | m: 'deposit |
                                            |             |
                                            +-------------+
                                            (cond ((eq? m 'withdraw)
                                                    withdraw)
                                                  ((eq? m 'deposit)
                                                    deposit)
                                                  (else
                                                    (error "..." m)))



          +----------------------------------------------------+
global -> |                                                    |
env       | make-account      acc                              |
          +----|---------------|-------------------------------+
               |       ^       |             ^
               |       |       |             |
               v       |       |   E1 -> +------------------+
             [*][*]----+       |         | balance: 50      |<----------+
              |                |         |                  |           |
              |                |         | withdraw --------------->[*][*]----> parameters: amount
              v                |         |                  |                   body: ...
  parameters: balance          |         |                  |<----------+
  body: (define withdraw ...)  |         |                  |           |
        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
        (define dispatch ...)  |         |                  |                   body: ...
        (lambda (m) ...)       |         |                  |<----------+
                               |         |                  |           |
                               +---------->dispatch --------------->[*][*]----> parameters: m
                                         |                  |                   body: ...
                                         +------------------+
                                                    ^
                                                    |
                                                    |
                                       (deposit 40) |
                                                    |
                                            +------------+
                                            |            |
                                      E3 -> | amount: 40 |
                                            |            |
                                            +------------+
                                            (set! balance (+ balance amount))



          +----------------------------------------------------+
global -> |                                                    |
env       | make-account      acc                              |
          +----|---------------|-------------------------------+
               |       ^       |             ^
               |       |       |             |
               v       |       |   E1 -> +------------------+
             [*][*]----+       |         | balance: 90      |<----------+
              |                |         |                  |           |
              |                |         | withdraw --------------->[*][*]----> parameters: amount
              v                |         |                  |                   body: ...
  parameters: balance          |         |                  |<----------+
  body: (define withdraw ...)  |         |                  |           |
        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
        (define dispatch ...)  |         |                  |                   body: ...
        (lambda (m) ...)       |         |                  |<----------+
                               |         |                  |           |
                               +---------->dispatch --------------->[*][*]----> parameters: m
                                         |                  |                   body: ...
                                         +------------------+




          +----------------------------------------------------+
global -> |                                                    |
env       | make-account      acc                              |
          +----|---------------|-------------------------------+
               |       ^       |             ^
               |       |       |             |
               v       |       |   E1 -> +------------------+
             [*][*]----+       |         | balance: 90      |<----------+
              |                |         |                  |           |
              |                |         | withdraw --------------->[*][*]----> parameters: amount
              v                |         |                  |                   body: ...
  parameters: balance          |         |                  |<----------+
  body: (define withdraw ...)  |         |                  |           |
        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
        (define dispatch ...)  |         |                  |                   body: ...
        (lambda (m) ...)       |         |                  |<----------+
                               |         |                  |           |
                               +---------->dispatch --------------->[*][*]----> parameters: m
                                         |                  |                   body: ...
                                         +------------------+
                                                   ^
                                                   |
                                                   |
                                             +--------------+
                                             |              |
                                       E4 -> | m: 'withdraw |
                                             |              |
                                             +--------------+
                                             (cond ((eq? m 'withdraw)
                                                     withdraw)
                                                   ((eq? m 'deposit)
                                                     deposit)
                                                   (else
                                                     (error "...")))



          +----------------------------------------------------+
global -> |                                                    |
env       | make-account      acc                              |
          +----|---------------|-------------------------------+
               |       ^       |             ^
               |       |       |             |
               v       |       |   E1 -> +------------------+
             [*][*]----+       |         | balance: 90      |<----------+
              |                |         |                  |           |
              |                |         | withdraw --------------->[*][*]----> parameters: amount
              v                |         |                  |                   body: ...
  parameters: balance          |         |                  |<----------+
  body: (define withdraw ...)  |         |                  |           |
        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
        (define dispatch ...)  |         |                  |                   body: ...
        (lambda (m) ...)       |         |                  |<----------+
                               |         |                  |           |
                               +---------->dispatch --------------->[*][*]----> parameters: m
                                         |                  |                   body: ...
                                         +------------------+
                                                   ^
                                                   |
                                                   |
                                             +------------+
                                             |            |
                                       E5 -> | amount: 60 |
                                             |            |
                                             +------------+
                                            (if (>= balance amount)
                                                (begin (set! balance (- balance amount))
                                                       balance)
                                                "...")


          +----------------------------------------------------+
global -> |                                                    |
env       | make-account      acc                              |
          +----|---------------|-------------------------------+
               |       ^       |             ^
               |       |       |             |
               v       |       |   E1 -> +------------------+
             [*][*]----+       |         | balance: 30      |<----------+
              |                |         |                  |           |
              |                |         | withdraw --------------->[*][*]----> parameters: amount
              v                |         |                  |                   body: ...
  parameters: balance          |         |                  |<----------+
  body: (define withdraw ...)  |         |                  |           |
        (define deposit ...)   |         | deposit ---------------->[*][*]----> parameters: amount
        (define dispatch ...)  |         |                  |                   body: ...
        (lambda (m) ...)       |         |                  |<----------+
                               |         |                  |           |
                               +---------->dispatch --------------->[*][*]----> parameters: m
                                         |                  |                   body: ...
                                         +------------------+




          +---------------------------------------------------------------+
global -> |                                                               |
env       | make-account        acc2    acc                               |
          +----|-----------------|--------|-------------------------------+
               |       ^         |        |             ^
               |       |         |        |             |
               v       |         |        |   E1 -> +------------------+
             [*][*]----+         |        |         | balance: 30      |<----------+
              |                  |        |         |                  |           |
              |                  |        |         | withdraw --------------->[*][*]----> parameters: amount
              v                  |        |         |                  |                   body: ...
  parameters: balance            |        |         |                  |<----------+
  body: (define withdraw ...)    |        |         |                  |           |
        (define deposit ...)     |        |         | deposit ---------------->[*][*]----> parameters: amount
        (define dispatch ...)    |        |         |                  |                   body: ...
        (lambda (m) ...)         |        |         |                  |<----------+
                                 |        |         |                  |           |
                                 |        +---------->dispatch --------------->[*][*]----> parameters: m
                                 |                  |                  |                   body: ...
                                 |                  +------------------+
                                 |
                                 |
                                 |
                                 |  E6 -> +------------------+
                                 |        | balance: 100     |<----------+
                                 |        |                  |           |
                                 |        | withdraw --------------->[*][*]----> parameters: amount
                                 |        |                  |                   body: ...
                                 |        |                  |<----------+
                                 |        |                  |           |
                                 |        | deposit ---------------->[*][*]----> parameters: amount
                                 |        |                  |                   body: ...
                                 |        |                  |<----------+
                                 |        |                  |           |
                                 +--------->dispatch --------------->[*][*]----> parameters: m
                                          |                  |                   body: ...
                                          +------------------+







