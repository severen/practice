#lang racket

(provide fib)

;; Calculate the nth Fibonacci number for some natural number n.
(define (fib n)
  (define (go n a b)
    (if (= n 0)
        a
        (go (sub1 n) b (+ a b))))
  (go n 0 1))
