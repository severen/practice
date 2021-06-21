#lang racket

(provide prime? next-prime nth-prime)

;; Check if a natural number n is a prime number.
(define (prime? n)
  (define (go n c)
    (cond [(> c (integer-sqrt n)) #t]
          [(zero? (modulo n c)) #f]
          [else (go n (add1 c))]))
  (if (< n 2)
      #f
      (go n 2)))

;; Find the next prime number after n, where n is a natural number.
(define (next-prime n)
  (define (go n)
    (if (prime? n)
        n
        (go (add1 n))))
  (go (add1 n)))

;; Calculate the nth prime number for some natural number n.
(define (nth-prime n)
  (define (go n c p)
    (if (= c n)
        p
        (go n (add1 c) (next-prime p))))
  (go n 0 2))
