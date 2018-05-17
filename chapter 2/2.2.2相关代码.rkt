#lang racket
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))
; test
(define x (cons (list 1 2) (list 3 4)))
(length x)

(define (count-leaves items)
  (cond ((null? items) 0)
        ((not (pair? items)) 1) ; 需要考虑pair
        (else (+ (count-leaves (car items))
                 (count-leaves (cdr items))))))
; test
(count-leaves x)

(list x x)
(length (list x x))
(count-leaves (list x x))

(length (list x (cons 1 2)))
(count-leaves (list x (cons 1 2)))

(define nil '())
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

(define (map-scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

