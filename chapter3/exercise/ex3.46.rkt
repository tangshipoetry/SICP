#lang racket




  |   P1                             mutex                             P2
  |   |                                                                 |
  |   |                                                                 |
  |   |                                                                 |
  |   +----------------------------> false <----------------------------+
  |    test-and-set!                                       test-and-set!
  |          |                                                   |
  |          |                                                   |
  |          +--------------------->  true <---------------------+
  |    (begin (set-car! cell true)           (begin (set-car! cell true)
  |           false)                                false)
  |
  v
time






































