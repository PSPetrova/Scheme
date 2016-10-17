#lang racket
;reverse num 123->321
(define (reverse-int n)
  (define (countnum m)
  ( cond ((equal? m 0) 0)
         (else (+ (countnum (floor (/ m 10))) 1))))
 (cond ((equal? (countnum n) 1) n)
       (else (+ (* (remainder n 10) (expt 10 (- (countnum n) 1))) (reverse-int (floor (/ n 10)))))))
;is it palindrom 123, 12321
(define (palindrom? n)
  (equal? n (reverse-int n)))

;sums all delimers of number
(define (divisors-sum n)
  (define (divhelper i)
    (cond ((equal? i n) n)
          ((equal? (remainder n i) 0) (+ i (divhelper (+ i 1))))
          (else (divhelper (+ i 1)))))
  (divhelper 1))

; is it perfect- sum of delimers is equal to the number
(define (perfect? n)
  (equal? n (- (divisors-sum n) n)))

;is prime number
(define (prime? n)
  (equal? (+ n 1) (divisors-sum n)))

;
(define (increasing? n)
  (define (inchelper prev m)
    (cond ((equal? prev 0) #t)
          ((>= (remainder m 10) prev ) #f)
          (else (inchelper  (remainder m 10) (floor (/ m 10))))))
(inchelper 10 n))

; number to list
(define (numtolist n)
  (cond ((equal? n 0) '())
        (else (append  (numtolist (floor (/ n 10))) (list (remainder n 10))))))
;reverse list
(define (reverselist l)
  (cond ((empty? l) '())
        (else (append (reverselist (cdr l)) (list (car l))))))
;return the last n element of the list
(define (lastnel l n)
  (cond ((equal? n 0) l)
        ((equal? (- (length l) n) 0) l)
        (else (lastnel (cdr l)  n))))
;return the first n element of the list
(define (firstnel l n)
  (cond ((equal? n 0) '())
        (else (append (list (car l)) (firstnel (cdr l) (- n 1))))))
;is equal for list
(define (lists-equal? a b)
  (cond ((and (empty? a) (empty? b)) #t)
        ((not (equal? (length a) (length b))) #f)
        ((not (equal? (car a) (car b))) #f)
        (else (lists-equal? (cdr a) (cdr b)))))

;check if a ends with b
(define (ends-with? a b)
(lists-equal? (lastnel (numtolist a) (length (numtolist b))) (numtolist b)))

;substring check
(define (substr? a b)
  (define (helper p q)
    (cond ((< (length p) (length q)) #f)
         ((equal? (car p) (car q))
         (if (lists-equal? (firstnel p (length q)) q) #t (helper (cdr p) q)))
         (else (helper (cdr p) q))))
  (helper (numtolist a) (numtolist b)))

;occurrences
(define (occurrences a n)
 (define (helper alist)
   (cond ((empty? alist) 0)
         ((equal? n (car alist)) (+ 1 (helper (cdr alist))))
         (else (helper (cdr alist)))))
  (helper (numtolist a)))

;contains all char in second list
(define (contains? a b)
  (cond ((equal? b 0) #t)
        ((substr? a (remainder b 10)) (contains? a (floor (/ b 10))))
        (else #f)))

;accumulate
