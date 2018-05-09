#lang racket
; exercise 1.21 - 1.28

; 1.21
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divide? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divide? n divisor)
  (= (remainder n divisor) 0))
(define (square n) (* n n))

(smallest-divisor 199) ;199
(smallest-divisor 1999) ;1999
(smallest-divisor 19999) ;19999

; 1.22
; 题设提到的runtime primitive并不存在
(define runtime current-milliseconds)
(define (prime? n)
  (= (smallest-divisor n) n))
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f)) ; silence compiler's error report

(define (report-prime elapsed-time)
  (display " *** ")
  (display " (")
  (display elapsed-time)
  (display "ms)\n")
  #t)
(define (timed-prime-test-in-range left right)
  (define (helper count)
    (cond ((> count right) (begin
                             (newline)
                             (display "finish")))
          ((even? count) (helper (+ count 1)))
          (else (begin
                  (timed-prime-test count)
                  (helper (+ count 2))))))
  (helper left))

(timed-prime-test-in-range 1000 1100)
; 1001 ***  (0ms)
; 1003 ***  (0ms)
; 1005 ***  (0ms)
(timed-prime-test-in-range 10000 11100)
; 10001 ***  (0ms)
; 10003 ***  (0ms)
; 10005 ***  (0ms)
(timed-prime-test-in-range 100000 100100)
; 100001 ***  (0ms)
; 100003 ***  (0ms)
; 100005 ***  (1ms)
(timed-prime-test-in-range 1000000 1000100)
; 1000001 ***  (0ms)
; 1000003 ***  (1ms)
; 1000005 ***  (0ms)
; 根据输出的时间，看不出theta根号n的时间复杂度的，由于性能的原因，样本得给到更大才行

; 1.23
(define (next input)
  (if (= input 2)
      3
      (+ input 2)))
(define (new-smallest-divisor n)
  (new-find-divisor n 2))
(define (new-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divide? n test-divisor) test-divisor)
        (else (new-find-divisor n (next test-divisor)))))
(define (new-prime? n)
  (= (new-smallest-divisor n) n))
(define (new-timed-prime-test n)
  (newline)
  (display n)
  (new-start-prime-test n (runtime)))
(define (new-start-prime-test n start-time)
  (if (new-prime? n)
      (report-prime (- (runtime) start-time))
      #f)) ; silence compiler's error report
; 搞两个case意思一下
(timed-prime-test 100000001111) ; 26ms
(new-timed-prime-test 100000001111) ; 16ms
(timed-prime-test 1000000011113) ; 83ms
(new-timed-prime-test 1000000011113) ; 56ms

; 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-timed-prime-test n times)
  (newline)
  (display n)
  (fast-start-prime-test n times (runtime)))
(define (fast-start-prime-test n times start-time)
  (if (fast-prime? n times)
      (report-prime (- (runtime) start-time))
      #f)) ; silence compiler's error report
; 
(fast-timed-prime-test 100 10)
(fast-timed-prime-test 101 10)
(fast-timed-prime-test 100011113 100)

; 1.25
; 引入fast-expt相关代码
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))
; 题设代码如下
(define (hacker-expmod base exp m) ; 将原题设的函数名改成了hacker-expmod, 避免函数名冲突
  (remainder (fast-expt base exp) m))
; test
; (hacker-expmod 10 1000000000000000000000 7) ; 程序陷入停滞
(expmod 10 1000000000000000000000 7) ; 程序并不会陷入停滞
; 原题设的函数问题在于，由于scheme是以applicative-order evaluate procedure，导致
; 函数需要先将(fast-expt basa exp)整个值(记其为result)计算出来然后再运行
; (remainder result m)，显然当result值非常大时有溢出的可能另一方面这样的计算
; 也是不必要的，原expmod函数是通过反复将幂值除以2来达到计算模值的目的

; 1.26
; 简单来说由于schema是以applicative-order evaluate procedure，所以
; 在(remainder (* (expmod base (/ exp 2) m)
;              (expmod base (/ exp 2) m))
; 题设的函数比原expmod函数多运行了一次(expmod base (/ exp 2) m))
; 只要exp为偶数，题设的函数计算量比原函数计算量多一倍，直接导致
; theta(logn))计算复杂度退化为theta(n)

; 1.27
; 561, 1105, 1729, 2465, 2821, 6601都是合数
(fermat-test 561)  ; #t
(fermat-test 1105) ; #t
(fermat-test 1729) ; #t
(fermat-test 2465) ; #t
(fermat-test 2821) ; #t
(fermat-test 6601) ; #t

; 1.28
(define (new-expmod base exp m)
  (define (check-helper n m)
    (let ((x (remainder (square n) m)))
      (if (and (not (= n 1)) (not (= n (- m 1))) (= x 1))
          0
          x)))
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-helper (new-expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (new-expmod base (- exp 1) m))
                    m))))
(define (miller-rabin-test n a)
  (= (expmod a (- n 1) n) 1))
 
(define (mr-prime? n)
  (cond ((= n 2) #t)
        ((even? n) #f)
        (else
         (prime-helper n (- n 1)))))
 
(define (prime-helper n a)
  (cond ((= a 0) #t)
        ((miller-rabin-test n a) (miller-rabin-test n (- a 1)))
        (else #f)))
(mr-prime? 10)
(mr-prime? 13)