#lang racket
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; exercise 1.20
; mormal-order: (gcd 206 40) -> (gcd 40 (remainder 206 40)) -> (gcd (remainder 206 40) (remainder 40 (remainder 206 40))) -> ...
; 省略之后的步骤，在normal-order下procedure remainder被call了18次
; applicative-order: (gcd 206 40) -> (gcd 40 (remainder 206 40)) -> (gcd 40 6) -> (gcd 6 (remainder 40 6)) -> (gcd 6 4)
; -> gcd(4 remainder(6 4)) -> gcd(4 2) -> gcd(2 remainder(4 2)) -> gcd(2 0) -> 2
; 在applicative-order下procedure remainder被call了4次