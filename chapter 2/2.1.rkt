#lang racket
; 2018/05/12
; exercise 2.1

; 2.1
(define (make-rat n d)
  (define (maker n d g)
    (cons (/ n g) (/ d g)))
  (let ((g (gcd n d)))
    (if (< (* n d) 0)
        (maker (- (abs n)) (abs d) (abs g))
        (maker (abs n) (abs d) (abs g)))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (numer x)
  (car x))
(define (denom x)
  (cdr x))
; test
(print-rat (make-rat 4 -6))
(print-rat (make-rat -4 -6))
(print-rat (make-rat 1 -3))