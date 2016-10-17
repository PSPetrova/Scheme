#lang racket
(define (list-contains? lst a)
  (cond ((empty? lst) #f)
        ((equal? (car lst) a) #t)
        (else (list-contains? (cdr lst) a))))
(define (get-n-elem lst n)
  (cond ((empty? lst) "er")
        ((= n 0) (car lst))
        (else (get-n-elem (cdr lst) (- n 1)))))

(define (get-column mat n)
  (cond ((empty? mat) '())
        (else (append (list (get-n-elem (car mat) n)) (get-column (cdr mat) n)))))

(define (sum lst)
  (cond ((empty? lst) 0)
        (else (+ (car lst) (sum (cdr lst))))))

(define (mat-elem-match? mat n)
  (define (pred lst)
    (if (list-contains? lst n) 1
        0))
  (equal? (sum (map pred mat)) (length mat)))

(define (mat-col-match? mat n)
 (define (pred x)
   (cond ((mat-elem-match? mat x) 1)
         (else 0)))
  (map pred (get-column mat n))
  (equal? (sum (map pred (get-column mat n))) (length mat)))

(define (hasColumn mat)
  (define (helper a b)
  (cond ((> a b) #f)
        ((mat-col-match? mat a) #t)
  (else (helper (+ 1 a) b))))
  (helper 0 (- (length mat) 1)))


(define matrix '((1 2 3) (2 3 4) (5 67 2)))