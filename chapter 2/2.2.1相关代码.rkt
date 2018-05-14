#lang racket
(cons 1
      (cons 2
            (cons 3
                  (cons 4 null))))
; is equivalent to
(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))
one-through-four

(car one-through-four) ; 1
(cdr one-through-four) ; (2, 3, 4)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
; test
(list-ref one-through-four 0)
(list-ref one-through-four 2)

; square
(define squares (list 1 4 9 16 25))
(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
(define (iter-length items)
  (define (helper product count)
    (if (null? product)
        count
        (helper (cdr product) (+ count 1))))
  (helper items 0))
; test
(length one-through-four)
(iter-length one-through-four)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; test
(append one-through-four squares)
