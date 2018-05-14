#lang racket
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b
           (remainder a b))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (better-make-rat n d)
  (define (maker n d g)
    (cons (/ n g) (/ d g)))
  (let ((g (gcd n d)))
    (if (< (* n d) 0)
        (maker (- (abs n)) (abs d) (abs g))
        (maker n d g))))
  

(define (numer x)
  (car x))
(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x)
               (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x)
               (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(define neg-one-third (make-rat 1 -3))
(print-rat (add-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(print-rat (add-rat neg-one-third neg-one-third))

(print-rat (mul-rat neg-one-third neg-one-third))

(print-rat (make-rat 4 -6))
(print-rat (make-rat -4 -6))
(print-rat (make-rat 1 -3))

(print-rat (better-make-rat 1 -3))
(print-rat (better-make-rat -1 3))
(print-rat (better-make-rat -4 6))