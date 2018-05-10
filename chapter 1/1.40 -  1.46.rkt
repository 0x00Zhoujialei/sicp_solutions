#lang racket
; 2018/05/10
; exercise 1.40 - 1.46

; some preparation code
(define dx 0.000001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx))
          (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x)
          ((deriv g) x)))))
(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.0000001))
  (let ((next (f guess)))
    (if (close-enough? guess next)
        guess
        (fixed-point f next))))
(define (newton-method f guess)
  (fixed-point (newton-transform f)
               guess))

; 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))
; 1.41
(define (double f)
  (lambda (x)
    (f (f x))))
(define (inc n) (+ n 1))
; test
((double inc) 3)
(((double double) inc) 3)
(((double (double double)) inc) 3)
;
(((double (double double)) inc) 5) ; 21
;; 之前做题犯了一个naive的错误 认为
;; ((double (lambda (x) (double (double x)))) inc) 等于 ((double (double (double inc))))
;; 而事实上(double (lambda (x) (double (double x))))是一个整体

; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
; test
(define (square x) (* x x))
((compose square inc) 6)

; 1.43
(define (repeated f n)
  (define (helper product count)
    (if (= count n)
        product
        (helper (compose product f)
                (+ count 1))))
  (helper f 1))
; test
((repeated square 2) 5)

; 1.44
(define (average a b c)
  (/ (+ a b c)
     3))
(define (smooth f)
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))
(define (n-fold-smoothed-function f n)
  (repeated (smooth f) n))

; 1.45
(define (average-damp f)
  (lambda (x)
    (/ (+ (f x) x)
       2)))
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))
(define (pow base exp)
  (cond ((= exp 1) base)
        ((even? exp) (pow (square base) (/ exp 2)))
        (else (* base (pow base (- exp 1))))))
(define (nth-root base total-repeat n)
  (fixed-point (repeated (average-damp (lambda (y) (/ base (pow y (- n 1))))) total-repeat)
               1.0))
; (nth-root (pow 2 8) 2 8)
; 1.9999999965451807, 惊了，显然这个结果是可以接受的
; (nth-root 256 1 9)
; 1.8517494119370557, 如果这样也能出结果，显然题设出了点什么问题。。。
; test

(define (average-damp-n-times f n)
    ((repeated average-damp n) f))
(define (damped-nth-root n damp-times)
    (lambda (x)
        (fixed-point 
            (average-damp-n-times 
                (lambda (y) 
                    (/ x (expt y (- n 1)))) 
                damp-times)
            1.0)))
(pow 2 3)
(pow 2 1)   
((repeated (average-damp (lambda (y) (/ 3 y))) 1) 2)


; 1.46
(define (iterative-improve f g)
  (define (helper guess)
    (if (f guess)
        guess
        (helper (g guess))))
  (lambda (guess)
    (helper guess)))
(define (iter-impro-sqrt x first-guess)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                     (lambda (guess) (new-average guess (/ x guess)))
                     ) first-guess))
(define (iter-impro-fixed-point f first-guess)
  ((iterative-improve (lambda (guess) (< (abs (- guess (f guess))) 0.0000001))
                       f)
    first-guess)) 

(define (new-average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
x)))