#lang racket

(provide prime? next-prime nth-prime)

;; Check if a natural number n is a prime number.
(define (prime? n)
  (define (go n c)
    (cond [(> c (integer-sqrt n)) #t]
          [(zero? (modulo n c)) #f]
          ;; We take advantage of the fact that the only prime numbers with a
          ;; gap less than 2 are 2 and 3, which are already covered
          ;; conditionally.
          [else (go n (+ c 2))]))
  (cond [(< n 2) #f]
        [(= n 2) #t]
        [(and (> n 2) (zero? (modulo n 2))) #f]
        [else (go n 3)]))

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
