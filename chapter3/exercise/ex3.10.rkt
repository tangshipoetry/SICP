#lang racket


(define (make-withdraw1 initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw2 initial-amount)
  ((lambda(balance)
     (lambda(amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient funds")))
   initial-amount))



(define make-withdraw
  (lambda(initial-amount)
    ((lambda(balance)
       (lambda(amount)
         (if (>= balance amount)
             (begin (set! balance (- balance amount))
                    balance)
             "Insufficient funds")))
     initial-amount)))


;----------------------------------------------------------------------------------------------------------------------------------------------------------
              +------------------------------------+
global env -> |                                    |
              |   make-withdraw --+                |
              +-------------------|----------------+
                                  |       ^
                                  |       |
                                  v       |
                                [*][*]----+
                                 |
                                 |
                                 v
                        parameters: initial-amount
                        body: ((lambda (balance)
                                   (lambda (amount)
                                       (if (>= balance amount)
                                           (begin (set! balance (- balance amount))
                                                  balance)
                                           "Insufficient funds")))
                               initial-amount)


              +------------------------------------+
global env -> |                                    |
              |                                    |
              +------------------------------------+
                     ^
(make-withdraw 100)  |
                     |
                 +--------------+
                 |              |
          E1 ->  | initial: 100 |
                 |              |
                 +--------------+

                ((lambda (balance)
                     (lambda (amount)
                         (if (>= balance amount)
                             (begin (set! balance (- balance amount))
                                    balance)
                             "Insufficient funds")))
                 initial)

              +------------------------------------+
global env -> |                                    |
              |                                    |
              +------------------------------------+
                     ^
(make-withdraw 100)  |
                     |
                 +--------------+
                 |              |
          E1 ->  | initial: 100 |
                 |              |
                 +--------------+
                   |        ^
                   |        |
                   |        |
               [*][*]-------+
                |
                |
                v
        parameters: balance
        body: (lambda (amount)
                  (if (>= balance amount)
                      (begin (set! balance (- balance amount))
                             balance)
                      "Insufficient funds"))


              +------------------------------------+
global env -> |                                    |
              |                                    |
              +------------------------------------+
                     ^
(make-withdraw 100)  |
                     |
                 +--------------+
                 |              |
          E1 ->  | initial: 100 |
                 |              |
                 +--------------+
                              ^
((lambda (balance) ...) 100)  |
                              |
                        +--------------+
                        |              |
                 E2 ->  | balance: 100 |
                        |              |
                        +--------------+

                       (lambda (amount)
                           (if (>= balance amount)
                               (begin (set! balance (- balance amount))
                                      balance)
                               "Insufficient funds"))


              +------------------------------------+
global env -> |                                    |
              |                                    |
              +------------------------------------+
                     ^
(make-withdraw 100)  |
                     |
                 +--------------+
                 |              |
          E1 ->  | initial: 100 |
                 |              |
                 +--------------+
                              ^
((lambda (balance) ...) 100)  |
                              |
                        +--------------+
                        |              |
                 E2 ->  | balance: 100 |
                        |              |
                        +--------------+
                           |        ^
                           |        |
                           v        |
                         [*][*]-----+
                          |
                          |
                          v
                   parameters: amount
                   body: (if (>= balance amount)
                             (begin (set! balance (- balance amount))
                                    balance)
                             "Insufficient funds")

             +-------------------------------------------+
global env -> |                                           |
              |   w1                                      |
              +---|---------------------------------------+
                  |                               ^
                  |          (make-withdraw 100)  |
                  |                               |
                  |                    +--------------+
                  |                    |              |
                  |             E1 ->  | initial: 100 |
                  |                    |              |
                  |                    +--------------+
                  |                               ^
                  | ((lambda (balance) ...) 100)  |
                  |                               |
                  |                    +--------------+
                  |                    |              |
                  |             E2 ->  | balance: 100 |
                  |                    |              |
                  |                    +--------------+
                  |                      |        ^
                  |                      |        |
                  |                      v        |
                  +------------------> [*][*]-----+
                                        |
                                        |
                                        v
                                 parameters: amount
                                 body: (if (>= balance amount)
                                           (begin (set! balance (- balance amount))
                                           balance)
                                       "Insufficient funds")




              +-------------------------------------------+
global env -> |                                           |
              |   w1                                      |
              +---|---------------------------------------+
                  |                               ^
                  |          (make-withdraw 100)  |
                  |                               |
                  |                    +--------------+
                  |                    |              |
                  |             E1 ->  | initial: 100 |
                  |                    |              |
                  |                    +--------------+
                  |                               ^
                  | ((lambda (balance) ...) 100)  |
                  |                               |
                  |                    +--------------+
                  |                    |              |
                  |             E2 ->  | balance: 100 |
                  |                    |              |
                  |                    +--------------+
                  |                      |        ^  ^
                  |                      |        |  |                                    +------------+
                  |                      v        |  |                                    |            |
                  +------------------> [*][*]-----+  +------------------------------------| amount: 50 | <- E3
                                        |                                                 |            |
                                        |                                                 +------------+
                                        v
                                 parameters: amount                                    (if (>= balance amount)
                                 body: (if (>= balance amount)                             (begin (set! balance (- balance amount))
                                           (begin (set! balance (- balance amount))               balance)
                                           balance)                                        "Insufficient funds")
                                       "Insufficient funds")

              +-------------------------------------------+
global env -> |                                           |
              |   w1                                      |
              +---|---------------------------------------+
                  |                               ^
                  |          (make-withdraw 100)  |
                  |                               |
                  |                    +--------------+
                  |                    |              |
                  |             E1 ->  | initial: 100 |
                  |                    |              |
                  |                    +--------------+
                  |                               ^
                  | ((lambda (balance) ...) 100)  |
                  |                               |
                  |                    +--------------+
                  |                    |              |
                  |             E2 ->  | balance: 50  |
                  |                    |              |
                  |                    +--------------+
                  |                      |        ^
                  |                      |        |
                  |                      v        |
                  +------------------> [*][*]-----+
                                        |
                                        |
                                        v
                                 parameters: amount
                                 body: (if (>= balance amount)
                                           (begin (set! balance (- balance amount))
                                           balance)
                                       "Insufficient funds")


              +-----------------------------------------------------------------------------------------+
global env -> |                                                                                         |
              |   w1                                        w2                                          |
              +---|-----------------------------------------|-------------------------------------------+
                  |                               ^         |                               ^
                  |          (make-withdraw 100)  |         |                               |
                  |                               |         |                               |
                  |                    +--------------+     |                      +--------------+
                  |                    |              |     |                      |              |
                  |             E1 ->  | initial: 100 |     |               E1 ->  | initial: 100 |
                  |                    |              |     |                      |              |
                  |                    +--------------+     |                      +--------------+
                  |                               ^         |                               ^
                  | ((lambda (balance) ...) 100)  |         | ((lambda (balance) ...) 100)  |
                  |                               |         |                               |
                  |                    +--------------+     |                      +--------------+
                  |                    |              |     |                      |              |
                  |             E2 ->  | balance: 50  |     |               E2 ->  | balance: 100 |
                  |                    |              |     |                      |              |
                  |                    +--------------+     |                      +--------------+
                  |                      |        ^         |                          |       ^
                  |                      |        |         |                          |       |
                  |                      v        |         |                          v       |
                  +------------------> [*][*]-----+         +----------------------->[*][*]----+
                                        |                                             |
                                        |                                             |
                                        v                                             v
                         parameters: amount                             parameters: amount
                         body: (if (>= balance amount)                  body: (if (>= balance amount)
                                   (begin (set! balance                           (begin (set! balance
                                                (- balance amount))                      (- balance amount))
                                          balance)                                balance)
                                   "Insufficient funds")                          "Insufficient funds")


