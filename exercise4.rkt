#lang racket
;check if element is in the list
(define (elem? el lst)
  (cond ((empty? lst) #f)
        ((equal? el (car lst)) #t)
        (else (elem? el (cdr lst)))))
;reverse list
(define (reverselst lst)
  (cond ((empty? lst) '())
        (else (append (reverselst (cdr lst)) (list (car lst))))))
;remove first el in list
(define (remove-first el lst)
  (cond ((empty? lst) '())
        ((equal? el (car lst)) (cdr lst))
        (else (append (list (car lst)) (remove-first el (cdr lst))))))
;remove all el in list
(define (remove-all el lst)
  (cond ((empty? lst) '())
        ((equal? el (car lst)) (remove-all el (cdr lst)))
        (else (append (list (car lst)) (remove-all el (cdr lst))))))
;map function
(define (map func lst)
  (cond ((empty? lst) '())
        (else (append (list (func (car lst))) (map func (cdr lst))))))
;filter function pred list
(define (filter pred? lst)
  (cond ((empty? lst) '())
        ((pred? (car lst)) (append (list (car lst)) (filter pred? (cdr lst))))
        (else (filter pred? (cdr lst)))))
;extract ints in list
(define (extract-ints lst)
  (filter number? lst))
;function that sum all el in list
(define (sum lst)
  (cond ((empty? lst) 0)
        (else (+ (car lst) (sum (cdr lst))))))
;sum all element in diferent lists
(define (sum-of-sums lst)
 (sum (map sum lst)))

;Upr6
(define (atom? x) (not (or (pair? x) (null? x))))
;zad1
(define (flatern-list lst)
  (cond 
    ((atom? lst) (list lst))
    ((empty? lst) '())
    (else (append (flatern-list (car lst)) (flatern-list (cdr lst))))))

(define (count-atoms lst)
  (length (flatern-list lst)))
;sum of all atoms in lists
(define (sum-atoms lst)
  (sum (filter atom? (flatern-list lst))))


(define (make-tree node left right)
  (list node left right))

(define (make-leaf node)
  (make-tree node '() '()))

(define (empty-tree? tree)
  (null? tree))

(define (root tree)
  (car tree))

(define (left tree)
  (car (cdr tree)))

(define (right tree)
  (car (cdr (cdr tree))))

(define (count-nodes tree)
  (cond
    [(empty? tree) 0]
    [else (+ 1 (count-nodes (left tree)) (count-nodes (right tree)))]))