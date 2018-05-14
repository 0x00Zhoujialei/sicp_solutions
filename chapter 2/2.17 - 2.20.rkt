#lang racket
; 2018/05/14
; exercise 2.17 - 2.20

; 2.17
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))
; test
(define list1 (list 1 2 3 4))
(define list2 (list 5 6 7 8))
(define list3 (list 2))

(last-pair list1)
(last-pair list2)
(last-pair list3)

; 2.18
(define (reverse items)
  (if (null? items)
      null
      (cons (reverse (cdr items)) (car items))))
; test
(reverse (list 1 2 3 4))

; 2.19
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (first-denomination coin-values)
  (car coin-values))
(define (no-more? coin-values)
  (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))
; test
(define us-coins (list 50 25 10 5 1))
(define us-coins-another-form (list 25 50 10 5 1))

(cc 100 us-coins) ; 292
(cc 100 us-coins-another-form) ; 292
; 改变coin-values里面的顺序当然不会影响cc的结果
; 因为给定的amount下不同的找零方法数量是固定的

; 2.20
(define (same-parity x . y)
  (define (same? a b)
    (= 0 (remainder (+ a b) 2)))
  (define (same-filter items)
    (cond ((null? items) null)
          ((same? x (car items)) (cons (car items)
                                       (same-filter (cdr items))))
          (else (same-filter (cdr items)))))
  (cons x (same-filter y)))
; test
(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
