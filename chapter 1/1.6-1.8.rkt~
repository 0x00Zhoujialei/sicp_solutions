#lang racket
; 2018/05/06
; exercise 1.6 - exercise 1.8

; 1.6
; 程序会一直执行下去一直到内存耗尽
; 解释：注意到new-if是一个有三个参数的函数，scheme以applicative-order执行函数的
; 即先evaluate参数，再将结果代入到原函数的procedure中,
; 注意到第三个参数调用了函数本身,便容易得到调用一次sqrt-iter必定会调用sqrt-iter这样一个结论
; 程序会一直执行下去便是显而易见的了

; 1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (square x) (* x x))
; test old good-enough
(sqrt 1000000)
(square (sqrt 1000000))
; very small number
(sqrt 0.00000001) ; 0.03125010656242753, support to be 0.0001
(square (sqrt 0.00000001)) ; 0.000976569160163076, support to be 0.00000001
; very large number
(sqrt 100000000000000000000000000000) ; 程序陷入停滞
; 对y = x^2 求导得 y' = 2x，
; 当x非常大时，函数增长非常快，此时sqrt需要长时间的运算才能得出结果
; 当x非常小时，函数几乎不增长，sqrt给不出正确结果

; 1.7 改进的good-enough
(define (new-sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (new-sqrt-iter (improve guess x)
                     x)))
(define (new-good-enough? guess x)
  (< (/ (changed guess x)
        guess)
     0.001))
(define (changed guess x)
  (abs (- (improve guess x) guess)))
(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))
; test 改进的good-enough
(new-sqrt 9) ; 3.00009155413138
(new-sqrt 0.00000001) ; 0.00010000040611237676
(square (new-sqrt 0.00000001)) ; 1.000008122264028e-08
(new-sqrt 100000000000000000000000000000) ; 316228179033787.4

; 1.8
(define (cube-root guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-root (cube-improve guess x)
                 x)))
(define (cube-good-enough? guess x)
  (< (/ (cube-changed guess x)
        guess)
     0.00001))
(define (cube-changed guess x)
  (abs (- (cube-improve guess x) guess)))
(define (cube-improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))
(define (cube-root-impl x)
  (cube-root 1.0  x))

; test 1.8
(cube-root-impl 9)
(cube-root-impl 8)
(cube-root-impl (* 7 7 7))
(cube-root-impl (* 11 11 11))

