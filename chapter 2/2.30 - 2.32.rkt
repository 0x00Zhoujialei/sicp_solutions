#lang racket
; 2018/05/15
; exercise 2.30 - 2.32

; 2.30
(define (square x) (* x x ))
(define nil '())
(define (square-tree tree)
  (cond ((null? tree) nil) ; 注意要先判断null? 再判断pair?
        ((not (pair? tree)) (square tree)) ; 因为(pair? '())的值是#f
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
(define (map-square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-square-tree sub-tree)
             (square sub-tree)))
       tree))
; test
(define tree1 (list 1 2 3 4 5 6))
(square-tree tree1)
(map-square-tree tree1)
(define tree2 (list 1 (list 2 3 (list 4 5))))
(square-tree tree2)
(map-square-tree tree2)

; 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
; test
(define (square-tree-map tree) (tree-map square tree))
(newline)
(square-tree-map tree1)
(square-tree-map tree2)

; 2.32
; 简单的说，对于一个非空集合F中的一个元素a,
; 这个F的子集可以分为含有a的子集和不含有a的子集
; 问号中要我们填入的procedure便是生成含有a的子集的过程
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))
; test
(define set1 (list 1))
(define set2 (list 1 2))
(define set3 (list 1 2 3))
(newline)
(subsets set1)
(subsets set2)
(subsets set3)