#lang simply-scheme
(define (id x)
  x)
(define (sq x)
  (* x x))
(define (cube x)
  (* x x x))
(define (inc x)
  (+ x 1))
(define (inc2 x)
  (+ x 2))
(define (dec x)
  (- x 1))
(define (sum-squares a b)
  (if (> a b)
      0
      (+ (sq a) (sum-squares (+ a 1) b))))
(define (average x y)
  (/ (+ x y) 2))
(define (even? x)
  (= (remainder x 2) 0))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))
                 ))
        )
  )

(define (fib-iter n)
  (define (internal-calc a b count)
    (if (< count n)
        (internal-calc (+ a b) a (+ count 1))
        b)
    )
  (internal-calc 1 0 0)
  )

(define (troco vl)
  (define (cc valor moedas)
    (define (tipo-de-moeda md)
      (cond ((= md 5) 50)
            ((= md 4) 25)
            ((= md 3) 10)
            ((= md 2) 5)
            ((= md 1) 1)
            )
      )
    (cond ((or (< valor 0) (= moedas 0)) 0)
          ((= valor 0) 1)
          (else (+ (cc valor
                       (- moedas 1))
                   (cc (- valor (tipo-de-moeda moedas))
                       moedas))
                )))
  (cc vl 5))

; 1.12
(define (pascal-triang linha elemento)
  (define (calcula l e)
    (if (or (= l e) (= e 1)) 1
        (+ (calcula (- l 1) (- e 1))
           (calcula (- l 1) e))))
  (calcula linha elemento))

;1.16
(define (iter-exp n b)
  (define (calc n b a)
    (cond ((= n 0) a)
          ((even? n) (calc (/ n 2) (sq b) a))
          (else (calc (dec n) b (* a b)))))
  (calc n b 1))

(define (double a)
  (+ a a))
(define (halve a)
  (if (even? a)
      (/ a 2)
      (error "can't halve odds")))

; 1.17
(define (r-* a b)
  (cond ((= a 1) b)
        ((even? a) (double (r-* (halve a) b)))
        (else (+ b (r-* (dec a) b)))))
; 1.18
(define (iter-* a b)
  (define (calc v a b)
    (cond ((= a 0) v)
          ((even? a) (calc v (halve a) (double b)))
          (else (calc (+ v b) (dec a) b))))
  (calc 0 a b))


(define (sum a b prox form)
  (define (iter valor i)
    (if (> i b)
        valor
        (iter (+ valor (form i))
              (prox i))))
  (iter 0 a))

;    (define (calc valor a b prox form)
;      (if (> a b)
;          valor
;          (calc (+ valor
;                   (form a)) (prox a) b prox form)))
;    (calc 0 a b prox form))

(define (integral a b func)
  (define dx 0.000001)
  (* (sum (+ a (/ dx 2.0))
          b
          (lambda (x) (+ x dx))
          func)
     dx))

(define pi 3.141592653589793)

(define (simpson a b f num)
  (define n (* num
               2.0))
  (define h (/ (- b a)
               n))
  (define (y k)
    (f (+ a (* k h))))
  (define (inc2 x) (+ 2 x))
  (* (/ h 3.0)
     (+ (y 0)
        (* 4 (sum 1 n inc2 y))
        (* 2 (sum 2 (- n 2) inc2 y))
        (y n))))

(define (product a b step f)
  (define (iter valor i)
    (if (> i b)
        valor
        (iter (* valor (f i)) (step i))))
  (iter 1 a))

(define (recur-product a b step f)
  (if (> a b)
      1
      (* (f a) (recur-product (step a) b step f))))

(define (factorial x)
  (recur-product 1 x inc id))

(define (wallis-piover4 n)
  (define b (+ n 1))
  (/ (* (product 1 b inc2 inc)
        (product 2 b inc2 inc2))
     (* (product 1 b inc2 inc2)
        (product 2 b inc2 inc))))

(define (accumulate combiner null-value term a next b)
  (define (iter valor i)
    (if (> i b)
        valor
        (iter (combiner valor (term i)) (next i))))
  (iter null-value a))

(define (r-accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (r-accumulate combiner null-value term (next a) next b))))

(define (a-product a b step f)
  (accumulate * 1 f a step b))

(define (a-sum a b step f)
  (r-accumulate + 0 f a step b))

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (combine-filtered combiner value term i filter)
    (if (filter i)
        (combiner value (term i))
        value))
  (define (iter valor i)
    (if (> i b)
        valor
        (iter (combine-filtered combiner valor term i filter) (next i))))
  (iter null-value a))

(define (par a)
  (define x
    (if (> a 0)
        a
        (- a)))
  (cond
    ((equal? 0 x) #t)
    ((not (> x 2)) (equal? x 2))
    (else (par (- x 2)))))


(define tolerance 0.00000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
         (if (close-enough? guess next)
             next
             (try next))))
  (try first-guess))

(define (fixed-point-print f first-guess)
  (define (println print)
    (display print)
    (newline))
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
         (println next)
         (if (close-enough? guess next)
             next
             (try next))))
  (try first-guess))

(define (cont-frac n d k)
  (define (frac x i)
    (/ (n i) (+ (d i) x)))
  (define (calc x i)
    (cond ((equal? i k)	(calc (frac 0 i) (dec i)))
          ((> i 0) (calc (frac x i) (dec i)))
          (else x)))
  (calc 0 k))

(define (r-cont-frac n d k)
  (define (frac x i)
    (/ (n i) (+ (d i) x)))
  (define (calc i)
    (if (equal? i k)
        (frac 0 i)
        (frac (calc (inc i)) i)))
  (calc 1))

(define (eulerexp k)
  (define (d i)
    (define (check3 i)
      (define (check n)
        (if (< n i)
            (check (+ 3 n))
            (equal? n i)))
      (check 2))
    (if (check3 i)
        (* 2 (/ (inc i) 3))
        1))
  (+ 2 (cont-frac (lambda (x) 1.0) d k)))

(define (tan-cf x k)
  (define (n i)
    (if (equal? i 1)
        x
        (- (sq x))))
  (define (d i) (dec (* 2 i)))
  (cont-frac n d k))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqroot x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (npower n x)
  (define (calc i total)
    (if (equal? i 0)
        total
        (calc (dec i) (* total x))))
  (calc n 1))

(define (deriv g)
  (define dx 0.000001)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (sq x)) (* b x) c)))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (build i g)
    (cond ((equal? 0 i)	g)
          ((< i 0) (error "can't apply"))
          ;(else (build (dec i) (compose g g)))))
          (else	(build (dec i) (lambda (x) (f (g x)))))))
  (build n id))

(define (smooth f)
  (define dx 0.01)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (nsmooth n f)
  ((repeated smooth n) f))

; nrt 2 4-7 nrt 1 1-3 nrt 3 8-15
(define (nrt n x)
  (define (dampfactor i)
    (define (calc j)
      (if (< i (* 4 j))
          j
          (calc (inc j))))
    (calc 1))
  (fixed-point ((repeated average-damp (dampfactor n)) (lambda (y) (/ x (npower (dec n) y))))
               1.0))

(define (iterative-improve good improve)
  (define (calc x)
    (if (good x)
        x
        (calc (improve x))))
  calc)

(define (sqrt-ii x)
  (define (good-enough? guess x)
    (< (abs (- (sq guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  ((iterative-improve (lambda (guess) (good-enough? guess x))
                      (lambda (guess) (improve guess x)))
   1.0))

(define (fixed-point-ii f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  ((iterative-improve (lambda (g) (close-enough? g (f g)))
                      f)
   first-guess))

