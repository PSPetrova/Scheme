#lang racket
;accumulate func
(define (accumulate op null-value term a next b)
  (if (> a b) null-value
      (op (term a) (accumulate op null-value term (next a) next b))))
;accumulate with filter
(define (accumulate-filter pred? op null-value term a next b)
 (cond ((> a b) null-value)
       ((pred? a) (op (term a) (accumulate-filter pred? op null-value term (next a) next b)))
       (else (accumulate-filter pred? op null-value term (next a) next b))))
;factoriel with accumulate
(define (fact-accum n)
  (define (term a)
    a)
  (define (next b)
    (+ b 1))
(accumulate * 1 term 1 next n))

;exponential with accumulate
(define (expt-accum x n)
  (define (term a)
    x)
  (define (next y)
    (+ 1 y))
(accumulate * 1 term 1 next n))

;sums all even numbers
(define (sumallev-accum n)
  (define (term a)
    a)
  (define (next b)
    (+ b 1))
  (define (pred? c)
    (even? c))
 (accumulate-filter pred? + 0 term 0 next n))
;count divisors in num
(define (count-divisors n a b)
  (define (term x)
   (if (equal? (remainder n x) 0) 1
       0))
  (define (next y)
    (+ 1 y))
  (accumulate + 0 term a next b))

;sum x+2x^2+3x^3+...+nx^n
(define (powers-sum x n)
  (define (term a)
    (* a (expt x a)))
  (define (next b)
    (+ 1 b))
  (accumulate + 0 term 1 next n))

; check if number is prime
(define (prime-accum? n)
 (equal? (count-divisors n 1 n) 2))

;is it perfect with accumulate
(define (perfect-accum? n)
  (define (term a)
    (if (equal? (remainder n a) 0) a
    0))
  (define (next b)
    (+ 1 b))
  (equal? (accumulate + 0 term 1 next (- n 1)) n))
;is number prime
(define (prime? n)
  (equal? (count-divisors n 1 n) 2))

;count num in number
(define (countnum n)
  (cond ((equal? n 0) 0)
        (else (+ 1 (countnum (floor (/ n 10)))))))
;reverse the number
(define (reversenum n)
  (cond ((equal? n 0) 0)
        (else (+ (* (remainder n 10) (expt 10 (- (countnum n) 1))) (reversenum (floor (/ n 10)))))))
;palindrom number
(define (palindrom? n)
  (equal? n (reversenum n)))
;zad backwardsPrime
(define (backwardsPrime a b)
  (cond ((> a b) '())
        ((and (prime? a) (prime? (reversenum a)) (not (palindrom? a))) (cons a (backwardsPrime (+ 1 a) b)))
        (else (backwardsPrime (+ 1 a) b))))
;suma s accumulate
(define (seriesSum n)
  (define (term a)
    (/ 1 (expt 3 (- a 1))))
  (define (next m)
    (+ 1 m))
  (accumulate + 0 term 1 next n))