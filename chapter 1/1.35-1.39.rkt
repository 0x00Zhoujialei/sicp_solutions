#lang racket
; 2018/05/09
; exercise 1.35 - 1.39

(define tolerance 0.000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try-it guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          guess
          (try-it next))))
  (try-it first-guess))

; 1.35
; 证明过程从略，令f(x) = 1 + 1/x, 代入x = (1+5^0.5)/2可得f(x) = x
; 自然证明了golden ratio是f(x)的fixed point
(exact->inexact (fixed-point (lambda (x) (+ 1 (/ 1 x)))
                             2)) ; 1.618034447821682

; 1.36
(define (fixed-point-for-prints f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try-it guess)
    (let ((next (f guess)))
      (begin (newline)
             (display guess))
      (if (close-enough? guess next)
          guess
          (try-it next))))
  (try-it first-guess))
(fixed-point-for-prints (lambda (x) (/ (log 1000) (log x))) 
                        4) ; 4.555535271282659, 4.555535271282659^4.555535271282659约等于999.9989

; 1.37
(define (cont-frac n d k)
  (define (first-product i)
    (+ (d (- i 1)) (/ (n i) (d i))))
  (define (reduc-result i p)
    (+ (d (- i 1)) (/ (n i) p)))
  (define (helper count product)
    (if (= count 1)
        (/ (n 1) product)
        (helper (- count 1) (reduc-result count product))))
  (helper (- k 1) (first-product k)))
; test
; 所求值为0.618033989
(cont-frac  (lambda (i) 1.0)
            (lambda (i) 1.0)
            10)  ; 0.6179775280898876
(cont-frac  (lambda (i) 1.0)
            (lambda (i) 1.0)
            11)  ; 0.6180555555555556
(cont-frac  (lambda (i) 1.0)
            (lambda (i) 1.0)
            12)  ; 0.6180257510729613
(cont-frac  (lambda (i) 1.0)
            (lambda (i) 1.0)
            13)  ; 0.6180371352785146
; k等于11时才满足小数点后4位的精度

;b
(define (cont-frac-recur n d k)
  (define (helper count)
    (if (= count k)
        (/ (n count) (d count))
        (/ (n count) (+ (d count) (helper (+ count 1))))))
  (helper 1))
; test
(cont-frac-recur  (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  10)  ; 0.6179775280898876
(cont-frac-recur  (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  11)  ; 0.6180555555555556
(cont-frac-recur  (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  12)  ; 0.6180257510729613
(cont-frac-recur  (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  13)  ; 0.6180371352785146

; 1.38
(define (e-minus-2 k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) (cond ((= i 1) 1)
                               ((= i 2) 2)
                               ((= (remainder i 3) 2)
                                (+ (* (/ 2 3) i)
                                   (/ 2 3)))
                               (else 1)))
             k))
; test
(newline)
(e-minus-2 10)
(e-minus-2 100)

; 1.39
(define (square x) (* x x))
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (square x))))
             (lambda (i) (- (* 2 i) 1))
             k))
; test
(tan-cf (/ pi 4) 10) ; 1.0