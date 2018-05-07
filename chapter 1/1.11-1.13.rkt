#lang racket
; 2018/05/07
; exercise 1.11 - 1.13
; 1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))
(define (f-iter n)
  (define (helper a b c index)
    (if (< n 3)
        n
        (if (= index n)
            c
            (helper b
                    c
                    (+ c (* 2 b) (* 3 a))
                    (+ index 1)))))
  (helper 0 1 2 2))
; 1.12
; 记col为pascal三角从1开始的行序数，col为元素所在行从左往右数从1开始的序数
(define (pascal row col) 
  (if (or (= row col) (= col 1))
      1
      (+ (pascal (- row 1) (- col 1))
         (pascal (- row 1) col))))
; 1.13
; 证明略过，得到提示中的结果后，要证明Fib(n)与(𝞿^n)/5的差的绝对值小于等于0.5
; 即证明(𝟁^n)/√5的值小于等于0.5，最后问题转换为证明幂函数f(n) = ((√5-1)/2)^n
; 在n大于等于0的区域上恒小于等于√5/2,结合幂函数的性质不难得到结论