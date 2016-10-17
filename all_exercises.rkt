#lang racket

(define ( fact x )

(if (= x 1) 1
    (* x (fact (- x 1))))
)

(define (factIter x)
(define (iter count product)
(if (> count x) product
    (iter (+ 1 count) (* count product))))
  (iter 1 1))

(define (power b n)
(if (= n 0) 1
    (* b (power b (- n 1) )))
  )

(define (powerIter b n )
 (define (iter counter product)
   (if (> counter n) product
       (iter (+ 1 counter) (* product b))
   ))
  (iter 1 1))
(define (proc a b)
 (if (= b 0) 0
     (+ a(proc a (- b 1)))))



(define (procIter a b)
 (define (iter counter sum)
     (if (> counter b) sum
         (iter (+ counter 1) (+ sum a))))
		 (iter 1 0))




(define (suma n)
 (if (= n 2) (- 1 (power 2 2))
     (*(- 1 (power n 2))(suma(- n 1)))))


(define (sumaIter n)
  (define (iter counter sum)
    (if (> counter n) sum
        (iter (+ counter 1) (* sum (- 1 (power counter 2))))))
 (iter 2 1))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib(- n 1))(fib(- n 2))))
        ))

(define (fibIter n)
  (define (iter counter prev cur)
    (if (= counter n) prev
        (iter (+ counter 1) cur (+ cur prev ) )))
  (iter 0 0 1 ))

(define (evenPow b n)
(cond ((even? n) (power (power b (/ n 2)) 2))
      ((= n 1) b)
      (else ( * b (power b (- n 1))))))


(define (cProd n m)

(define (iter  count prod)

(if (< count m) prod
    (iter (+ 1 count) (* prod (+ (- n count) 1)))
  ))
(iter 0 1))


(define (first x)
  (/ 1(* x(+ x 2))))
(define (sum_pi a b)
  (if(> a b) 0
     (+ (first a)
        (sum_pi (+ a 4) b))))



(define (accumulate combiner null_value term a next b)
  (if (> a b) null_value
      (combiner (term a)
         (accumulate combiner null_value term (next a) next b))))


(define (V n m)
  (define (term x)
    (+(- n x) 1) )
  (define (next x)
    (+ 1 x))
  
  (accumulate * 1 term 1 next m))

(define (C n m)
  (define (term x)
    (/ ( + (- n x) 1) x))
  
  (define (next x)
    (+ 1 x))
 (accumulate * 1 term 1 next m))


(define (filter_accum pred combiner null_value term a next b)
  (cond ((> a b) null_value)
        ((pred (term a))
          (combiner (term a) (filter_accum pred combiner null_value term (next a) next b)))
        (else(filter_accum pred combiner null_value term (next a) next b) )))


(define (zad2a n)
  (define (next x)
    (+ 1 x))
  (define (term x)
    x)
  (define (pred x)
    (and (even? x) (odd? (/ x 2)))
    )
  (filter_accum pred * 1 term 1 next n )
  )

(define (zad6b x n)
  (define (term y)
     (/
      (* (power (- 1) y)
         (power y
                (+ 1 (* n 2)))
         )
      (fact (+ (* 2 n) 1))))
  (define (next y)
    (+ 1 y))
  (accumulate + 0 term 0 next n))



(define (func f n)
  (if (= n 1) (f n)
      (f ( func f (- n 1)))))

(define ( oper x)
(+ 1 x))


(define (funcL n)
((lambda (x)
           (if (= n 1) (oper n)
           (oper (funcL oper ( - n 1)))))
		   n))

(define (test x)
  ((lambda (number) (+ number 3)) x))

(define (addit x)
( + x 2))

(define (repeater f n)
(lambda (x)
        (if (= n 1) (f x)
            (f ((repeater f ( - n 1)) x)))))
  

;((repeater addit 3) 4)

(define (test2 num)
  (lambda (x y) (+ x y num)))

(define (1a n)
  (define (f x)
    (+(power x 2)1))
  (repeater f n))
 
(define (1b n)
 (define (f x)
   (power x 3))
  (repeater f n))
  
(define (spis)
  '())

(define (spisi n lisi)
  (if (= n 1) (car lisi)
      (spisi (- n 1) (cdr lisi) )
      ))

(define (length g)
 (if (null? g) 0
     (+ 1 (length(cdr g)))))


(define (last_ot spisak)
  (if (null? (cdr spisak)) (car spisak)
      (last_ot (cdr spisak)))
  )
(define (append x y)
 (if (null? x) y
     (cons (car x) (append (cdr x) y))))

(define (reverse x)
 (if (null? (cdr x)) x
      (append (reverse (cdr x)) (list (car x)))))

(define (atom? x) (not (or (pair? x) (null? x))))

(define (countatoms spisak)
  (cond ((null? spisak) 0)
        ((atom? spisak) 1)
        (else (+ (countatoms (car spisak))
                 (countatoms(cdr spisak))))))


(define (maketree v l r) (list v l r))
(define (root t) (car t))
(define (left t) (cadr t))
(define (right t) (caddr t))
(define (empty? t) (null? t))


; !!! много по-лесно с взимане на второто ниво (!)
(define (children t)
  (if (null? t)
      '()
     (append (if (not (null? (left t)))
                 (list (root (left t)))
                 '())
          (if (not (null? (right t)))
              (list (root (right t)))
              '()))))
              
(define (grandchildren t)
  (append (if (not (null? (left t)))
              (children (left t))
              '())
          (if (not (null? (right t)))
              (children (right t))
              '())))
;(grandchildren '(1 2 3 4 5 6 7 8 9))
(define (add7 x)
  (+ x 1))
(define (add8 x)
  (+ x 2))
(define (add9 x)
  x)
(define (smetka x f h) (abs (- (h x) (f x))))

(define (aproximate f l)
 (lambda (x)
   (define ( helper h sp div)
     (cond ( (null?  (cdr sp)) h)
           ( ( < (smetka x f h) div )
                      ( helper (car sp) (cdr sp) (smetka x f h) ))
           (else ( helper h (cdr sp) div ))))
    (helper (car l) l 0)))

;((aproximate add9 '(add7 add8)) 1)
(define (approximate f l)
  (lambda (x ) ((minAbs (car l) f l) x)))
  
   
(define (minAbs min f l)
 (lambda (x )
  (cond ((null? l) min)
    ((< (abs (- (f x) ((car l) x))) (abs (- (f x) (min x)))) (minAbs (car l) f (cdr l)))
    (minAbs min f (cdr l)))))


(define (fun f x y)
 (eq? (f x) y))

(define (iter1 f lis)
  (define (iter prev lis)
   (cond ((null? (cdr lis)) #t)
         ((eq? (fun f prev (car lis)) #f) #f)
          (else (iter (car lis) (cdr lis)))))
     (iter (car lis) (cdr lis)))

(define (vrashta spis)
  (cond ((null? spis) '())
        ((even? (car spis)) (append (list(car spis)) (vrashta (cdr spis))))
        (else (vrashta (cdr spis)))))


(define (join  x)
  (cond ( (null? x) x)
        ( (atom? x) (list x))
        (else (append (join (car x)) (join (cdr x))))))





(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))