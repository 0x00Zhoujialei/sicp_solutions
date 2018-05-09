#lang racket
; 2018/05/09
; exercise 1.34

(define (f g)
  (g 2))
(define (square n) (* n n))
(f square)
(f (lambda (z) (* z (+ z 1))))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; (f f)

; (f (lambda (g)
;        (g 2)))
; ->
; ((lambda (g)
;       (g 2))
; ->
; (lambda (g)
;      (g 2)))
; ->
; ((lambda (g)
;    (g 2))
; 2)
; -> 注意lambda函数的定义 这个时候2是作为g的body内的值代入 的
; (2 2)
; 显然(2, 2)不合法