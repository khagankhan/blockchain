#lang racket
;; Racket implementation of Bob Jenkins’ ISAAC pseudo-random number generator: http://burtleburtle.net/bob/rand/isaacafa.html
(require irandom)
(require math/number-theory)
(provide (all-defined-out))
;; Generate large odd numbers that are more likely to be prime.
(define (generate-prime-numbers)
  (let ([number (irandom-32)])
    (if (prime? number)
        number (generate-prime-numbers))))
;; TO-DO: Write your own primality check algorithm:
;; Miller-Robin? Fermat? Baillie-PSW?
;; Ref: https://lists.racket-lang.org/users/archive/2005-July/009259.html
;; Ref: https://github.com/racket/math/blob/master/math-lib/math/private/number-theory/number-theory.rkt
;; It does the sieve of Eratosthenes for small numbers, and use
;; (multiple rounds of) strong pseudoprimality test for larger numbers.
;; Generate two coprime numbers.
(define (generate-coprime-numbers)
  (let ([num1 (generate-prime-numbers)]
         [num2 (generate-prime-numbers)])
  (if (coprime? num1 num2) ;; Any two prime numbers are coprime. I added this as an additional check.
      (list num1 num2) 
      (error "[-] Error: Malformed algorithm when getting prime numbers"))))
;; Generate modulus n and Euler's Totient
(define (n-and-fi)
  (let* ([primes (generate-coprime-numbers)]
         [first-number (list-ref primes 0)]
         [second-number (list-ref primes 1)]
         [n (* first-number second-number)]
         [fi (* (- first-number 1) (- second-number 1))])
    (list n fi)))
;; Generate Public exponent
(define (public-exponent)
  (let* ([n (list-ref (n-and-fi) 0)]
         [fi (list-ref (n-and-fi) 1)]
         [e (random-prime fi)])
    (if (coprime? e fi) ;; Any two prime numbers are coprime. I added this as an additional check.
        (list e n fi)
        (error "[-] Error: Malformed algorithm when getting public exponent"))))
;; Getting private exponent
(define (generate-public-private-pairs)
  (let* ([e (list-ref (public-exponent) 0)]
         [n (list-ref (public-exponent) 1)]
         [fi (list-ref (public-exponent) 2)]
         [bez (bezout e fi)]
         [d (list-ref bez 0)])
     (if (< d 0)
         (generate-public-private-pairs)
         (if (= 1 (remainder (* e d) fi)) 
             (list (list e n) (list d n)) 
             (error "[-] Error: Malformed algorithm when getting private exponent")))))
;;(generate-public-private-pairs)
;; Generate public and private pairs
#|
(define num irandom-fixnum)
(if (even? num) 5 6)
error:
```
even?: contract violation
  expected: integer?
  given: #<procedure:irandom-fixnum>
```
|#