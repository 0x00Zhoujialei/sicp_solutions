#lang racket
; 2018/05/07
; exercise 1.9 - 1.10

; 1.9
; 举例说明, (+ 2 4)
; 对于前者 (+ 2 4) -> (inc (+ 1 4)) -> (inc (inc (+ 0 4))) -> (inc 5) -> 6
; 对于后者 (+ 2 4) -> (+ 1 5) -> (+ 0 6) -> 6
; 显然前者为recursive后者为iterative

; 1.10
; (A 1 10) -> (A 0 (A 1 9)) -> (A 0 (A 0 (A 1 8))) -> .... -> (A 0 (A 0 (A 0 ... (A 1 1)))) -> (A 0 (A 0 ... (A 0 2)))(含有9个(A 0...)这样的嵌套) -> 1024(2 ^ 10)
; (A 2 4) -> (A 1 (A 2 3)) -> (A 1 (A 1 (A 2 2))) -> (A 1 (A 1 (A 1 (A 2 1)))) -> (A 1 (A 1 (A 1 2))) -> (A 1 (A 1 (A 0 (A 1 1)))) -> (A 1 (A 1 (A 0 2))) -> (A 1 (A 1 4)) -> (A 1 (A 0 (A 1 3))) -> (A 1 (A 0 (A 0 (A 1 2)))) -> (A 1 (A 0 .. (A 0 (A 1 1)))) -> (A 1 16) -> 65536(2 ^ 16)
; (A 3 3) -> (A 2 (A 3 2)) -> (A 2 (A 2 (A 3 1))) -> (A 2 (A 2 2)) -> (A 2 (A 1 (A 2 1))) -> (A 2 (A 1 2)) -> (A 2 (A 0 (A 1 1))) -> (A 2 (A 0 2)) -> (A 2 4) -> 65536(2 ^ 16)

; (f n) computes 2n
; (g n) computes 2^n
; (h n) computes 2 ^ (2 ^ (2 ^ ... 2))，求n次2次幂,图就不上了,很容易归纳出
; (A 2 n) -> (A 1 (A 2 (- n 1))) -> (A 1 (A 1 (A 2 (- n 2)))) -> ... -> (A 1 (A 1 ... (A 2 1))) -> (A 1 (A 1 ... (A 1 2)))
; 记函数嵌套层数为t, 最内层函数第三个参数的值为v，有
; t = n - 1, v = 2
; t = n - 2, v = 4 = 2 ^ 2
; t = n - 3, v = 16 = 2 ^ (2 ^ 2)
; t = n - 4, v = 2 ^ 16 
; 归纳一下不难得出结果，证明略过
