#lang racket
; 2018/05/14
; exercise 2.21 - 2.23

; 2.21
(define (square x) (* x x))
(define nil '())

(define (square-list-one items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list-one (cdr items)))))

(define (square-list-two items)
  (map square items))

; test
(square-list-two (list 1 2 3 4))
(square-list-one (list 1 2 3 4))

; 2.22
; 第一问以(list 1 2 3 4)为例我们来分步解释结果
; answer -> '(1, nil) -> '(4, (1, nil)) -> '(9, (4, (1, nil))) -> '(16, (9, (4, (1, nil))))
; 第二问
; answer -> '(nil, 1) -> '((nil, 1), 4) -> '(((nil, 1), 4), 9) -> '((((nil, 1), 4), 9), 16)
; 这显然也不是我们要的结果

; 2.23
(define (for-each p lst)
    (if (not (null? lst))
        (begin
            (p (car lst))
            (for-each p (cdr lst)))
        (newline)))

; test
(for-each (lambda (x) (newline) (display x))
          (list 1 2 3))
(for-each (lambda (x) (newline) (display x) (display (+ x 1)))
          (list 1 2 3))