#lang racket
; 0 1 1 2 3 5 8
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fib-iter n)
  (define (helper a b index)
    (cond ((= n index) a)
          ((= n (+ index 1)) b)
          (else (helper b (+ a b) (+ index 1)))))
  (helper 0 1 0))

(define (fib-count a b count)
  (if (= count 0)
      a
      (fib-count b (+ a b) (- count 1))))
(define (fib-new-iter n)
  (fib-count 0 1 n))

(define (count-change n)
  (cc n 5))
(define (cc count kind-of-coins)
  (cond ((= count 0) 1)
        ((or (< count 0)
             (= kind-of-coins 0)) 0)
        (else (+ (cc count
                     (- kind-of-coins 1))
                 (cc (- count
                        (first-denomination kind-of-coins))
                     kind-of-coins)))))
(define (first-denomination kind-of-coins)
  (cond ((= kind-of-coins 5) 50)
        ((= kind-of-coins 4) 25)
        ((= kind-of-coins 3) 10)
        ((= kind-of-coins 2) 5)
        ((= kind-of-coins 1) 1)))