#lang racket
; exercise 1.16 - 1.19

; 1.16
(define (fast-expt-iter b n)
	(define (helper b n a)
		(cond ((= n 0) a)
			  ((even? n) (helper (* b b)
			  					 (/ n 2)
			  					 a ))
			  (else (helper b (- n 1) (* a b)))))
	(helper b n 1)
	)

; 1.17
; prerequest
(define (double a)
	(* a 2))
(define (halve a)
	(/ a 2))
; 
(define (fast-multiply-by-add a b)
	(define (helper a b count)
		(cond ((= b 0) count)
			  ((even? b) (helper (double a)
			  					 (halve b)
			  					 count))
			  (else (helper a (- b 1) (+ count a))))
	)
	(helper a b 0))

; 1.18
; 正巧1.17我的impl就是iterative的，所以两道题用1.17的代码就满足要求

; 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))