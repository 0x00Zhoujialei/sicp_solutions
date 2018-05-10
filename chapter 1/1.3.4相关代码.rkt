#lang racket
(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.0001))
  (let ((next (f guess)))
    (if (close-enough? guess next)
        guess
        (fixed-point f next))))

(define (average x y)
  (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (let ((first-guess 1.0))
  (fixed-point (average-damp (lambda (y) (/ x y)))
               first-guess)))
(sqrt 3)

(define (square x) (* x x))
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newtons-method-sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average-damp-sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
(define (newton-transfrom-sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transfrom
                            1.0))
