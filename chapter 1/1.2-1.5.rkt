#lang racket
; 2018/05/05
; exercise 1.2 - exercise 1.5

; 1.2
(/ (+ 5 4
        (- 2
           (- 3
              (+ 6
                 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

; 1.3
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x)
     (square y)))
(define (larger-numbers-sqrt-sum x y z)
  (cond ((and (>= x z) (>= y z)) (sum-of-squares x y))
        ((and (>= x y) (>= z y)) (sum-of-squares x z))
        (else (sum-of-squares y z))))
; test 1.3
(larger-numbers-sqrt-sum 3 2 1)
(larger-numbers-sqrt-sum 1 2 3)
(larger-numbers-sqrt-sum 2 3 1)
(larger-numbers-sqrt-sum 7 5 2)
(larger-numbers-sqrt-sum 8 8 9)

; 1.4
; a-plus-abs-b 即a加上b的绝对值，需要注意的是这里通过(if (> b 0) + -)来动态
; 决定了是使用+函数还是-函数, scheme是支持函数作为值传递的

; 1.5 解答
; 如果是applicative-order evaluation，
; 即先evaluate operands再将结果代入procedure中
; 那么程序会在evaluate p时由于(define (p) (p))陷入死循环
; 如果是normal-order evaluation, 即先将operand代入procedure再evaluate整个procedure
; 那么程序会正常结束，将原来的参数代入test函数有
; (if (= 0 0)
;    0
;    (p))
; (p)不会被evaluate 自然不会陷入死循环